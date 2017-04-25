library ieee ;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram_simulation is
	generic(
		tHZOE: time := 4ns;
		tLZOE: time := 0ns;
		tHZCE: time := 4ns;
		tLZCE: time := 3ns;
		tOHA: time := 2ns;
		tAA: time := 10ns;
		tDOE: time := 4.5ns;
		tSCE: time := 8ns;
		tAW: time := 8ns;
		tHA: time := 0ns;
		tSA : time := 0ns;
		tPWE1 : time := 8ns;
		tPWE2 : time := 10ns;
		tSD : time := 6ns;
		tHD : time := 0ns;
		tHZWE : time := 5ns;
		tLZWE : time := 2ns);
	port(
		ram_dio: inout std_logic_vector(23 downto 0) := (others => '0');
		ram_a: in std_logic_vector(18 downto 0);
		ram_we: in std_logic;
		ram_ce: in std_logic;
		ram_oe: in std_logic);
end entity ram_simulation;

architecture behavior of ram_simulation is
	type memory is array(0 to 2**17) of std_logic_vector(23 downto 0);
	signal ram: memory;
	signal addr: integer range 0 to 2**17;
	signal read_addr: integer range 0 to 2**17;
	signal ram_dout: std_logic_vector(23 downto 0);
	signal ram_din: std_logic_vector(23 downto 0);
	
	signal adr_change: std_logic := '0';
	
	signal hiz: std_logic;
	signal oe_z: std_logic;
	signal oe_hiz: std_logic;
	signal oe_loz: std_logic;
	signal ce_z: std_logic;
	signal ce_hiz: std_logic;
	signal ce_loz: std_logic;
	
	signal we_z: std_logic;
	signal we_hiz: std_logic;
	signal we_loz: std_logic;	
begin
	addr <= to_integer(unsigned(ram_a(16 downto 0)));
	
	oe_hiz <= ram_oe after tHZOE;
	oe_loz <= ram_oe after tLZOE;
	we_hiz <= not ram_we after tHZWE;
	we_loz <= not ram_we after tLZWE;
	process (all)
	begin
		if tHZOE < tLZOE then
			oe_z <= oe_hiz or oe_loz;
		else
			oe_z <= oe_hiz and oe_loz;
		end if;
	end process;
	
	process (all)
	begin
		if tHZWE < tLZWE then
			we_z <= we_hiz or we_loz;
		else
			we_z <= we_hiz and we_loz;
		end if;
	end process;
	
	process (all)
	begin
		if ram_a'event then
			ram_dout <= (others => 'U') after tOHA;
			adr_change <= not adr_change after tAA;
		end if;
		if adr_change'event then
			ram_dout <= ram(addr);
		end if;
	end process;
	
	ce_hiz <= ram_ce after tHZCE;
	ce_loz <= ram_ce after tLZCE;
	process (all)
	begin
		if tHZCE < tLZCE then
			ce_z <= ce_hiz or ce_loz;
		else
			ce_z <= ce_hiz and ce_loz;
		end if;
	end process;

	hiz <= oe_z or ce_z or we_z;
	ram_dio <= ram_dout when hiz='0' else (others => 'Z');
	
	process (all)
	begin
		for bits in 0 to 23 loop
			case ram_dio(bits) is
				when '0'|'L' => ram_din(bits) <= '0';
				when '1'|'H' => ram_din(bits) <= '1';
				when others => ram_din(bits) <= 'U';
			end case;
		end loop;
	end process;
	
	--ram_dout <= ram(read_addr) when read_oe='0' and read_ce='0' and ram_we='1' else (others => 'X');
	process (ram_we, ram_dio, ram_ce)
	begin
		if rising_edge(ram_we) then
			if ram_ce='0' then
				ram(addr) <= ram_din;
			end if;
		end if;
	end process;
end behavior;