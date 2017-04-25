library ieee ;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
----------------------------------------------------
entity uart_rx is
port(
	data_out: out std_logic_vector(7 downto 0);
	data_read: in std_logic := '0';
	data_read_empty: out std_logic;
	uart_clock: in std_logic;
	data_clock: in std_logic;
	serial_data_in: in std_logic := '0';
	reset: in std_logic
);
end entity uart_rx;
----------------------------------------------------
architecture behavior of uart_rx is
	type state is (
		idle,
		rx_start,
		rx_0, rx_1, rx_2, rx_3, rx_4, rx_5, rx_6, rx_7, rx_stop );
	signal status: state := idle;
	signal oversample_counter: INTEGER range 0 to 32 := 0;
	signal fifo_write: std_logic := '0';
	signal fifo_full: std_logic;
	signal fifo_empty: std_logic;
	signal rx_byte: std_logic_vector(7 downto 0);
	signal rx_error: std_logic := '0';
	
	signal din1: std_logic;
	signal din2: std_logic;
	signal din_actual: std_logic;
begin
	in_fifo: entity work.fifo_gen
		port map(
		Data => rx_byte,
		Q => data_out,
		WrEn => fifo_write,
		WrClock => uart_clock,
		RdClock => data_clock,
		RpReset => reset,
		RdEn => data_read,
		Reset => reset,
		Full => fifo_full,
		Empty => data_read_empty
		);
	process (uart_clock)
	begin
		if uart_clock'event and uart_clock='1' then
			din_actual <= din2;
			din2 <= din1;
			din1 <= serial_data_in;
			if din_actual='0' and status=idle then
				status <= rx_start;
				oversample_counter <= 0;
			elsif status /= idle and oversample_counter<31 then
				oversample_counter <= oversample_counter + 1;
			end if;
			if oversample_counter=7 then
				case status is
					when rx_0 => rx_byte(0) <= din_actual;
					when rx_1 => rx_byte(1) <= din_actual;
					when rx_2 => rx_byte(2) <= din_actual;
					when rx_3 => rx_byte(3) <= din_actual;
					when rx_4 => rx_byte(4) <= din_actual;
					when rx_5 => rx_byte(5) <= din_actual;
					when rx_6 => rx_byte(6) <= din_actual;
					when rx_7 => rx_byte(7) <= din_actual;
					when rx_stop => rx_error <= not din_actual;
					when others => null;
				end case;
			elsif oversample_counter=15 then
				oversample_counter <= 0;
				case status is
					when rx_start => status <= rx_0;
					when rx_0 => status <= rx_1;
					when rx_1 => status <= rx_2;
					when rx_2 => status <= rx_3;
					when rx_3 => status <= rx_4;
					when rx_4 => status <= rx_5;
					when rx_5 => status <= rx_6;
					when rx_6 => status <= rx_7;
					when rx_7 => status <= rx_stop;
					when rx_stop => 
						if serial_data_in='1' then
							status <= idle;
						else
							status <= rx_start;
						end if;
					when others => null;
				end case;
			elsif status=rx_stop and oversample_counter=8 then
				if fifo_full='0' and rx_error='0' then
					fifo_write <= '1';
				end if;
			elsif fifo_write='1' then
				fifo_write <= '0';
			end if;
		end if;
	end process;
end behavior;

library ieee ;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
----------------------------------------------------
entity uart_tx is
port(
	data_in: in std_logic_vector(7 downto 0);
	data_write: in std_logic;
	data_write_full: out std_logic;
	uart_clock: in std_logic;
	data_clock: in std_logic;
	serial_data_out: out std_logic;
	serial_transmitting: out std_logic;
	reset: in std_logic
);
end entity uart_tx;
----------------------------------------------------
architecture behavior of uart_tx is
	signal byte_transmit: std_logic_vector(7 downto 0) := "ZZZZZZZZ";
	signal shift_output: std_logic_vector(9 downto 0);
	signal shift_loaded: std_logic_vector(9 downto 0);
	signal transmit_mode: INTEGER range 0 to 14 := 0;
	signal fifo_read: std_logic := '0';
	signal fifo_empty: std_logic;
	signal fifo_outputs: std_logic;
	signal fifo_clock: std_logic;
	signal fifo_tx_clock: std_logic;
	signal tx_clock: std_logic;
begin
	tx_clk_gen: entity work.divide_by_n
		generic map ( factor => 8 )
		port map(
		input => uart_clock,
		output => tx_clock
		);
	fifo_clock <= not data_clock;
	in_fifo: entity work.fifo_gen
		port map(
		Data => data_in,
		Q => byte_transmit,
		WrEn => data_write,
		WrClock => fifo_clock,
		RdClock => fifo_tx_clock,
		RpReset => reset,
		RdEn => fifo_read,
		Reset => reset,
		Full => data_write_full,
		Empty => fifo_empty
		);
	fifo_outputs <= data_write;
	fifo_tx_clock <= not tx_clock;
	process (tx_clock, reset, shift_loaded, shift_output, fifo_empty, fifo_read)
	begin
		if reset='1' then
			shift_loaded <= (others => '0');
			shift_output <= (others => '1');
			serial_transmitting <= '0';
			fifo_read <= '0';
			serial_data_out <= '1';
		else
			if rising_edge(tx_clock) then
				if shift_loaded(0)='1' then
					shift_loaded <= '0' & shift_loaded(9 downto 1);
					serial_data_out <= shift_output(0);
					shift_output <= '1' & shift_output(9 downto 1);
				else
					shift_loaded <= '0' & shift_loaded(9 downto 1);
					serial_data_out <= shift_output(0);
					shift_output <= '1' & shift_output(9 downto 1);
				end if;
				if shift_loaded(9 downto 0)="0000000000" and fifo_empty='1' then
					serial_transmitting <= '0';
				end if;
				if fifo_read='1' then
					fifo_read <= '0';
					shift_loaded <= (others => '1');
					shift_output <= '1' & byte_transmit & '0';
				elsif shift_loaded(9 downto 2)="00000000" and fifo_empty = '0' then
					fifo_read <= '1';
					serial_transmitting <= '1';
				end if;
			end if;
		end if;
	end process;
end behavior;
-----------------------------------------------------



library ieee ;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
----------------------------------------------------
entity uart is
port(
	data_in: in std_logic_vector(7 downto 0);
	data_write: in std_logic := '0';
	data_write_full: out std_logic;
	data_out: out std_logic_vector(7 downto 0);
	data_read: in std_logic := '0';
	data_read_empty: out std_logic;
	uart_clock: in std_logic;
	tx_clock: in std_logic;
	rx_clock: in std_logic;
	serial_data_in: in std_logic := '0';
	serial_data_out: out std_logic;
	serial_transmitting: out std_logic := '0';
	reset: in std_logic
);
end entity uart;

architecture behavior of uart is
	
begin
	tx: entity work.uart_tx 
		port map(
		data_in => data_in,
		uart_clock => uart_clock,
		data_clock => tx_clock,
		data_write => data_write,
		data_write_full => data_write_full,
		serial_data_out => serial_data_out,
		serial_transmitting => serial_transmitting,
		reset => reset);
	rx: entity work.uart_rx
		port map(
		data_out => data_out,
		uart_clock => uart_clock,
		data_clock => rx_clock,
		data_read => data_read,
		data_read_empty => data_read_empty,
		serial_data_in => serial_data_in,
		reset => reset);
end behavior;