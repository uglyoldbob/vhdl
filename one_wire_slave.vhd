library ieee ;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
----------------------------------------------------
entity one_wire_slave is
--clock period is in nanoseconds
generic (clock_period: integer);
port(
	dout: out std_logic;
	din: in std_logic;
	clock: in std_logic;
	reset: in std_logic
);
end entity one_wire_slave;
----------------------------------------------------
architecture behavior of one_wire_slave is
	--the numbers below are all nanoseconds, divided by clock period, to get actual clock cycle count
	constant max_delay : integer := 500000 / clock_period;
	constant write_drive_time : integer := 57000 / clock_period;
	constant write_inactive_time : integer := 10000 / clock_period;
	constant reset_time : integer := 450000 / clock_period;
	constant presence_delay_time : integer := 10000 / clock_period;
	constant presence_drive_time : integer := 10000 / clock_period;
	constant reset_end_delay : integer := 50000 / clock_period;
	constant read_sample_delay_time : integer := 22000 / clock_period;
	constant read_inactive_time : integer := 45000 / clock_period;
	type state is (
		idle, write1_wait, write1_driving, write0_wait, write0_driving, write_waiting, 
		read_wait, read_sample_delay, read_sample, read_inactive,
		presence_delay, presence_drive, reset_end_wait
		);
	type low_level_command is (
		idle, write0, write1, read_bit, reset_modules);
	signal low_level_state: state;
	signal low_level_timer: integer range 0 to max_delay + 1;
	signal reset_timer: integer range 0 to max_delay + 1;
	signal calculated_dout: std_logic;
	signal last_read_bit: std_logic;
	signal devices_present: std_logic;
	
	--signals for driving the low level system
	signal low_level_idle : std_logic;
	signal low_level_request : low_level_command;
	
	signal previous_din : std_logic;
	signal falling_din : std_logic;
	signal rising_din : std_logic;
	
	signal reset2: std_logic;
	signal combined_reset: std_logic;
begin
	combined_reset <= reset and reset2;
	dout <= '1' when combined_reset='0' else calculated_dout;
	low_level_request <= read_bit;
	process (clock)
	begin
		previous_din <= din;
		falling_din <= previous_din and not din;
		rising_din <= not previous_din and din;
	end process;
	process (clock)
	begin
		if rising_edge(clock) then		
			if din='0' and reset2='1' and reset_timer < reset_time then
				reset_timer <= reset_timer + 1;
			elsif din='1' then
				reset_timer <= 0;
			end if;
			if reset_timer >= reset_time then
				reset2 <= '0';
			elsif din='1' then
				reset2 <= '1';
			end if;
		end if;
	end process;
	process (clock)
	begin
		if combined_reset='0' then
			low_level_state <= presence_delay;
			calculated_dout <= '1';
		elsif rising_edge(clock) then
			case low_level_state is
				when presence_delay =>
					calculated_dout <= '1';
					low_level_timer <= low_level_timer + 1;
					if (low_level_timer >= presence_delay_time) then
						low_level_state <= presence_drive;
						low_level_timer <= 0;
					end if;
				when presence_drive =>
					calculated_dout <= '0';
					low_level_timer <= low_level_timer + 1;
					if (low_level_timer >= presence_delay_time) then
						low_level_state <= reset_end_wait;
						low_level_timer <= 0;
					end if;
				when reset_end_wait =>
					calculated_dout <= '1';
					low_level_timer <= low_level_timer + 1;
					if (low_level_timer >= reset_end_delay) then
						low_level_state <= idle;
						low_level_timer <= 0;
					end if;
				when idle => 
					low_level_idle <= '1';
					calculated_dout <= '1';
					low_level_timer <= 0;
					case low_level_request is
						when write0 => low_level_state <= write0_wait;
						when write1 => low_level_state <= write1_wait;
						when read_bit => low_level_state <= read_wait;
						when others => low_level_state <= idle;
					end case;
				when write1_wait =>
					calculated_dout <= '1';
					if falling_din='1' then
						low_level_state <= write1_driving;
					end if;
				when write1_driving =>
					calculated_dout <= '1';
					low_level_timer <= low_level_timer + 1;
					if (low_level_timer >= write_drive_time) then
						low_level_state <= write_waiting;
						low_level_timer <= 0;
					end if;
				when write0_wait =>
					calculated_dout <= '1';
					if falling_din='1' then
						low_level_state <= write0_driving;
					end if;
				when write0_driving =>
					calculated_dout <= '0';
					low_level_timer <= low_level_timer + 1;
					if (low_level_timer >= write_drive_time) then
						low_level_state <= write_waiting;
						low_level_timer <= 0;
					end if;if din='0' then
						low_level_state <= write1_driving;
					end if;
				when write_waiting =>
					calculated_dout <= '1';
					low_level_timer <= low_level_timer + 1;
					if (low_level_timer >= write_inactive_time) then
						low_level_state <= idle;
						low_level_timer <= 0;
					end if;
				when read_wait =>
					calculated_dout <= '1';
					if falling_din='1' then
						low_level_state <= read_sample_delay;
					end if;
				when read_sample_delay =>
					low_level_timer <= low_level_timer + 1;
					if (low_level_timer >= read_sample_delay_time) then
						low_level_state <= read_sample;
						low_level_timer <= 0;
					end if;
				when read_sample =>
					last_read_bit <= din;
					low_level_state <= read_inactive;
				when read_inactive =>
					calculated_dout <= '1';
					low_level_timer <= low_level_timer + 1;
					if (low_level_timer >= read_inactive_time) then
						low_level_state <= idle;
						low_level_timer <= 0;
					end if;
				when others => 
					low_level_idle <= '0';
					calculated_dout <= '1';
					low_level_timer <= 0;
					low_level_state <= idle;
			end case;
		end if;
	end process;
end behavior;
