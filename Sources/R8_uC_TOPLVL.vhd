-------------------------------------------------------------------------------------------------------------
-- Design unit: R8_uC_TOPLVL
-- Description: Instantiation of R8 microcontroller
-- Author: Carlos Gewehr and Emilio Ferreira (cggewehr@gmail.com, emilio.ferreira@ecomp.ufsm.br)
------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

entity R8_uC_TOPLVL is
	port (
		clock      : in std_logic;                        -- 100MHz board clock (Pin V10)
		reset      : in std_logic;                        -- Asynchronous reset (Board reset button, Switch 7 (left-most), pin T5
		port_io_uc : inout std_logic_vector(15 downto 0); -- OPEN (Must be configured according to application)
		uart_tx    : out std_logic;                       -- Serial transmitter (Pin N17)
        uart_rx    : in std_logic;                        -- Serial receiver (Pin N18)
        prog_mode  : in std_logic                         -- Processor mode selector (1 for programming RAM, 0 for executing code on RAM, Switch 0 (right-most), pin T10)
	);
end R8_uC_TOPLVL;

architecture Behavioural of R8_uC_TOPLVL is

    -- Basic signals
	signal clk_2, clk_4               : std_logic; -- 50MHz clock for uC and 25 MHz clock (unused)
	signal reset_sync                 : std_logic; -- Synchronous reset

begin

	-- Xilinx DCM
    ClockManager: entity work.ClockManager
        port map(
            clk_in   => clock,
            clk_div2 => clk_2,
            clk_div4 => clk_4
        );
        
    -- Reset Synchronizer    
    ResetSynchronizer: entity work.ResetSynchronizer
        port map(
            clk     => clk_2,
            rst_in  => reset,
            rst_out => reset_sync
        );

    -- R8 Microcontroller (R8 Processor, Memory, I/O Port, PIC, UART TX and RX)
    Microcontroller: entity work.R8_uC
    	generic map (
    	    RAM_IMAGE     => "AssemblyT7_display1ms_BRAM.txt",
    	    ROM_IMAGE     => "ROM_IMAGE_BRAM.txt",
            ADDR_PORT     => "0000",
            ADDR_PIC      => "1111",
            ADDR_UART_TX  => "1000",
            ADDR_UART_RX  => "1010",
            ADDR_TIMER    => "1011"
    	)
		port map (
    	    clk       => clk_2,
    		rst       => reset_sync,
    		port_io   => port_io_uC,
            uart_tx   => uart_tx,
            uart_rx   => uart_rx,
            prog_mode => prog_mode
    	);

end architecture Behavioural;
