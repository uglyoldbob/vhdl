library ieee ;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
entity ewma is
	generic(
		num_bits: integer := 16;
		num_a: std_logic_vector(7 downto 0) := "00001010";
		num_b: std_logic_vector(7 downto 0) := "00000111";
		denom: std_logic_vector(7 downto 0) := "00010001");
	port(
		clock: in std_logic;
		din: in std_logic_vector(num_bits-1 downto 0);
		dout: out std_logic_vector(num_bits-1 downto 0));
end entity ewma;
architecture behavior of ewma is
	signal prev_din: std_logic_vector(num_bits-1 downto 0);
	signal store_prev: std_logic_vector(num_bits-1 downto 0);
	signal store_cur: std_logic_vector(num_bits-1 downto 0);
	signal calc_prev: std_logic_vector(8+num_bits-1 downto 0);
	signal calc_cur: std_logic_vector(8+num_bits-1 downto 0);
	signal calc_combine: std_logic_vector(8+num_bits-1 downto 0);
	signal calc_divide: std_logic_vector(8+num_bits-1 downto 0);
begin
	process (all)
	begin
		if rising_edge(clock) then
			store_prev <= prev_din;
			store_cur <= din;
			dout <= prev_din;
		end if;
		calc_prev <= std_logic_vector(unsigned(store_prev) * unsigned(num_b));
		calc_cur <= std_logic_vector(unsigned(store_cur) * unsigned(num_a));
		calc_combine <= std_logic_vector(unsigned(calc_prev) + unsigned(calc_cur));
		calc_divide <= std_logic_vector(unsigned(calc_combine) / unsigned(denom));
		prev_din <= calc_divide(8+num_bits-1 downto 8);
	end process;
end behavior;