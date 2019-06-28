
-- VHDL Test Bench Created from source file top.vhd -- Wed May 24 11:24:06 2017

--
-- Notes: 
-- 1) This testbench template has been automatically generated using types
-- std_logic and std_logic_vector for the ports of the unit under test.
-- Lattice recommends that these types always be used for the top-level
-- I/O of a design in order to guarantee that the testbench will bind
-- correctly to the timing (post-route) simulation model.
-- 2) To use this template as your testbench, change the filename to any
-- name of your choice with the extension .vhd, and use the "source->import"
-- menu in the ispLEVER Project Navigator to import the testbench.
-- Then edit the user defined section below, adding code to generate the 
-- stimulus for your design.
-- 3) VHDL simulations will produce errors if there are Lattice FPGA library 
-- elements in your design that require the instantiation of GSR, PUR, and
-- TSALL and they are not present in the testbench. For more information see
-- the How To section of online help.  
--
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY one_wire_testbench IS
END one_wire_testbench;

ARCHITECTURE behavior OF one_wire_testbench IS 
	signal clock : std_logic := '0';
BEGIN

-- Please check and add your generic clause manually
	uut: entity work.one_wire_master 
	generic map(clock_period => 100)
	PORT MAP(
		clock => clock,
		reset => '1',
		din => '1'
	);
	clock <= not clock after 50ns;

-- *** Test Bench - User Defined Section ***
	tb : PROCESS
	BEGIN
		wait until rising_edge(clock);
		wait for 100ns;
	END PROCESS;
-- *** End Test Bench - User Defined Section ***

END;
