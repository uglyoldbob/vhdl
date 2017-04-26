library ieee; 
use ieee.std_logic_1164.all;

entity rising_edge_flag is
	port(
		din: in std_logic;
		clock: in std_logic;
		flag: out std_logic);
end entity rising_edge_flag;

architecture behavior of rising_edge_flag is
	signal delay: std_logic;
begin
	process (clock)
	begin
		if rising_edge(clock) then
			delay <= din;
		end if;
	end process;
	flag <= din and not delay;
end behavior;

library ieee; 
use ieee.std_logic_1164.all;

entity falling_edge_flag is
	port(
		din: in std_logic;
		clock: in std_logic;
		flag: out std_logic);
end entity falling_edge_flag;

architecture behavior of falling_edge_flag is
	signal delay: std_logic;
begin
	process (clock)
	begin
		if rising_edge(clock) then
			delay <= din;
		end if;
	end process;
	flag <= not din and delay;
end behavior;