library ieee ;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity cycle_delay is
	generic(
		num_bits: integer := 8;
		num_cycles: integer := 1);
	port (
		clock_in: in std_logic;
		din: in std_logic_vector(num_bits-1 downto 0);
		dout: out std_logic_vector(num_bits-1 downto 0));
end cycle_delay;

architecture behavior of cycle_delay is
	type sigs is array(num_cycles-1 downto 0) of std_logic_vector(num_bits-1 downto 0);
	signal int_sigs: sigs;
begin
	process (clock_in, din, int_sigs)
	begin
		if rising_edge(clock_in) then
			for i in 0 to num_cycles-2 loop
				int_sigs(i+1) <= int_sigs(i);
			end loop;
			int_sigs(0) <= din;
			dout <= int_sigs(num_cycles-1);
		end if;
	end process;
end behavior;

library ieee ;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity cycle_bit_delay is
	generic(
		num_cycles: integer := 1);
	port (
		clock_in: in std_logic;
		din: in std_logic;
		dout: out std_logic);
end cycle_bit_delay;

architecture behavior of cycle_bit_delay is
	signal int_sigs: std_logic_vector(num_cycles-1 downto 0);
begin
	process (clock_in, din, int_sigs)
	begin
		if rising_edge(clock_in) then
			for i in 0 to num_cycles-2 loop
				int_sigs(i+1) <= int_sigs(i);
			end loop;
			int_sigs(0) <= din;
			dout <= int_sigs(num_cycles-1);
		end if;
	end process;
end behavior;