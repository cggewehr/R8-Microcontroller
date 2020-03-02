--------------------------------------------------------------------------------
-- Project     : R8Gen
--------------------------------------------------------------------------------
-- File        : R8.vhd
-- Authors     : Carlos Gewehr(carlos.gewehr@ecomp.ufsm.br), 
--               Emilio Ferreira(emilio.ferreira@ecomp.ufsm.br)
-- Standard    : VHDL-1993
--------------------------------------------------------------------------------
-- Description :  R8 processor behavioural description, with features defined in
--               R8_PKG.vhd (software generated)
--------------------------------------------------------------------------------
-- Changelog   : v0.01 - Gewehr: Initial commit, defines project template
--------------------------------------------------------------------------------
-- TODO        : - Translate comments to english
--               - Clean up flag generation
--               - Adapt state machine to used constants defined in R8_PKG
--               - Review instruction decoding (not all bits fo regIR are needed)
--               - 
--------------------------------------------------------------------------------

library IEEE;
	use IEEE.std_logic_1164.all;
	use IEEE.std_logic_signed.all;
	use IEEE.numeric_std.all;

library work;
	use work.R8_PKG.all;

entity R8 is

    port(
		
		-- Basic
        Clock : in std_logic;
        Reset : in std_logic;

		-- Peripheral interrupt flag (will be left open if none are instantiated)
        InterruptionRequest : in std_logic;
        
        -- RAM programming flag (will be left open if programmer is not instantiated)
        ProgMode : in std_logic;

        -- Memory interface
        DataIn  : in std_logic_vector(15 downto 0);
        DataOut : out std_logic_vector(15 downto 0);
        Address : out std_logic_vector(15 downto 0);
        CE      : out std_logic;
        RW      : out std_logic

    );

end R8;


architecture Behavioural of R8 is

	-- FSM state and processor instructions
	signal currentState       : State_t;        -- Type defined in R8_PKG
	signal InstructionAndType : Instruction_t;  -- Type defined in R8_PKG
	alias Instruction         : R8_Instruction_t is InstructionAndType.Instruction; -- Type defined in R8_PKG
	alias InstructionType     : InstructionType_t is InstructionAndType.InstructionType;

	-- General purpose registers
	type RegisterArray is array(0 to 15) of std_logic_vector(15 downto 0);
    signal regBank : RegisterArray;

	-- ALU combinatinal signals
    signal ALUaux              : std_logic_vector(16 downto 0); -- Sinal com 17 bits pra lidar com overflow (soma de sinais de 16 bits)
    signal outALU              : std_logic_vector(15 downto 0);
	signal regBComplement      : std_logic_vector(15 downto 0);
	signal constanteExtended   : std_logic_vector(15 downto 0);
	signal constanteComplement : std_logic_vector(15 downto 0);

	-- Synchronous flag signals
    signal regFLAGS : std_logic_vector(3 downto 0);
    alias n         : std_logic is regFLAGS(3);
    alias z         : std_logic is regFLAGS(2);
    alias c         : std_logic is regFLAGS(1);
    alias v         : std_logic is regFLAGS(0);

    -- Combinational flag sinals
    signal flagN, flagZ, flagC, flagV : std_logic; -- Flags ALU| negativo, zero, carry, overflow

	-- Instruction register fields
	alias OPCODE      : std_logic_vector(3 downto 0) is regIR(15 downto 12);
	alias REGTARGET   : std_logic_vector(3 downto 0) is regIR(11 downto  8);
	alias REGSOURCE1  : std_logic_vector(3 downto 0) is regIR( 7 downto  4);
	alias REGSOURCE2  : std_logic_vector(3 downto 0) is regIR( 3 downto  0);
	alias CONSTANTE   : std_logic_vector(7 downto 0) is regIR( 7 downto  0);
	alias JMPD_AUX    : std_logic_vector(1 downto 0) is regIR(11 downto 10);
	alias JMPD_DESLOC : std_logic_vector(9 downto 0) is regIR( 9 downto  0);
	alias JSRD_DESLOC : std_logic_vector(11 downto 0) is regIR(11 downto  0);

	-- Interruption flags
	signal interruptFlag            : std_logic; -- Signals if processor is currently treating an interruption
    signal trapCount                : integer;   -- Signals if processor is currently treating a trap
    signal newTrapFlag              : std_logic; -- Signals if processor generated a new untreated trap
    signal nullPointerExceptionFlag : std_logic; -- Signals if processor is trying to read/write data (not reading an instruction) @ memory position 0
    
begin

    process(Clock, Reset) begin

		if Reset = '1' then

			-- Reset all processor registers (as defined on R8_PKG)
			ResetProcessorRegisters(ProcessorRegisters);

            -- Reset general purpose registers
            for i in 0 to 15 loop
                regBank(i) <= (others=>'0');
            end loop;

            -- Reset normal execution flow interrupting flags (interruptions or traps)
			interruptFlag <= '0';
            newTrapFlag <= '0';
            trapCount <= 0;

            -- Defines next state
            currentState <= Sfetch;

		elsif rising_edge(Clock) then

		    if currentState = Sfetch then  -- Gets next instruction from memory

	    		-- New trap detected
				if SupportsTraps and newTrapFlag = '1' then

                    -- Defines next state
					currentState <= Strap;

                    -- Saves address of instruction being executed (before interruption/trap)
                    regITR <= regPC;

                -- New interruption and not currently treating an interruption
                elsif SupportsInterruptions and InterruptionRequest = '1' and interruptFlag = '0' then

                    -- Defines next state
                    currentState <= Sitr;

                    -- Saves address of instruction being executed (before interruption/trap)
                    regITR <= regPC;

				else

					-- Defines next state
					currentState <= Sreg;

					-- Decodes instruction (DecodeInstruction defined in R8_PKG)
					regIR <= DataIn;
					Instruction <= DecodeInstruction(DataIn);

                    -- Increments Program Counter
                    regPC <= regPC + 1;

				end if;

            elsif currentState = Strap then

                -- Saves PC on stack
                regSP <= regSP - 1;

                -- InterruptFlag stays active until RTI instruction is executed (cant be interrupted by peripheral)
                interruptFlag <= '1';
                newTrapFlag <= '0';
                trapCount <= trapCount + 1;

                -- Next instruction is the first instruction on the TSR subroutine
                regPC <= regTSRA; -- Trap (exception/syscall, generated by software)

                -- Fetches first instruction of TSR subroutine
                currentState <= Sfetch;

            elsif currentState = Sitr then

                -- Saves PC on stack
                regSP <= regSP - 1;

                -- InterruptFlag stays active until RTI instruction is executed
                interruptFlag <= '1';

                -- Next instruction is the first instruction on the ISR subroutine
                regPC <= regISRA; -- Interruption (generated by peripheral/hardware)

                -- Fetches first instruction of ISR subroutine
                currentState <= Sfetch;

        	elsif currentState = Sreg then

                if Instruction = INVALID then
                    
                    if SupportsTraps then

                    	regCAUSE <= std_logic_vector(to_unsigned(1, regCAUSE'length)); -- Throws Invalid Instruction
                    	newTrapFlag <= '1';

                    end if;

                    currentState <= Sfetch;

                elsif Instruction = HALT then

                    currentState <= Shalt;

                else

                    -- Reads register bank
                    regA <= regBank(to_integer(unsigned(REGSOURCE1)));

                    if (InstructionType = Type2 or Instruction = PUSH or Instruction = MUL or Instruction = DIV) then
                        regB <= regBank(to_integer(unsigned(REGTARGET)));
                    else
                        regB <= regBank(to_integer(unsigned(REGSOURCE2)));
                    end if;

                    -- Defines next state
                    currentState <= Sula;

        		end if;

        	elsif currentState = Shalt then

        		if SupportsInterruptions then

        			-- Idles until next reset (only leaves this state when signal Reset = '1' or InterruptionRequest = '1')
                	if InterruptionRequest = '1' and interruptFlag = '0' then

                    	currentState <= Sitr;
                    	regPC <= regPC - 1;  -- In order to remain on halt on return from interruption

                    end if;

                else

                    currentState <= Shalt;

                end if;

        	elsif currentState = Sula then

        		-- Writes ALU combinational signal to regALU register (synchrounous)
                regALU <= outALU;

                -- Generates flags
                if Instruction = ADD or Instruction = SUB or Instruction = ADDI or Instruction = SUBI then

                    v <= flagV;  -- Overflow flag
                    c <= flagC;  -- Carry flag

                end if;

                if (InstructionType = Type1) or (Instruction = ADDI or Instruction = SUBI) then

                    n <= flagN; -- Negative flag
                    z <= flagZ; -- Zero flag

                end if;

          		-- Defines next state (based on current instruction)
          		if (InstructionType = Type1) or (InstructionType = Type2) then
          			currentState <= Swbk;

          		elsif Instruction = LD then
          			currentState <= Sld;

          		elsif Instruction = ST then
          			currentState <= Sst;

          		elsif InstructionType = Jump then

              		if Instruction = JMPR or Instruction = JMP or Instruction = JMPD then
                        currentState <= Sjmp;
                    elsif (Instruction = JMPNR or Instruction = JMPN or Instruction = JMPND) and n = '1' then
                        currentState <= Sjmp;
                    elsif (Instruction = JMPZR or Instruction = JMPZ or Instruction = JMPZD) and z = '1' then
                        currentState <= Sjmp;
                    elsif (Instruction = JMPCR or Instruction = JMPC or Instruction = JMPCD) and c = '1' then
                        currentState <= Sjmp;
                    elsif (Instruction = JMPVR or Instruction = JMPV or Instruction = JMPVD) and v = '1' then
                        currentState <= Sjmp;
                    else
                        currentState <= Sfetch;
                    end if;

          		elsif Instruction = JSR or Instruction = JSRR or Instruction = JSRD then
          			currentState <= Ssbrt;

          		elsif Instruction = PUSH then
          			currentState <= Spush;

          		elsif Instruction = RTS then
          			currentState <= Srts;

          		elsif Instruction = POP then
          		 	currentState <= Spop;

          		elsif Instruction = LDSP then
          			currentState <= Sldsp;


          		-- NOVAS INSTRUÇOES T3P2
          		elsif Instruction = PUSHF then
          			currentState <= Spushf;

          		elsif Instruction = POPF then
          			currentState <= Spopf;

          		elsif Instruction = RTI then
          			currentState <= Srti;


          		-- NOVAS INSTRUÇÔES T4P1
          		elsif Instruction = MUL then
          			currentState <= Smul;

          		elsif Instruction = DIV then
          			currentState <= Sdiv;

          		elsif Instruction = MFH then
          		    currentState <= Smfh;

          		elsif Instruction = MFL then
          			currentState <= Smfl;


                -- NOVA INSTRUÇÂO T4P2
                elsif Instruction = LDISRA then
                    currentState <= Sldisra;


                -- NOVAS INSTRUÇÔES T5P2
                elsif Instruction = LDTSRA then
                    currentState <= Sldtsra;

                elsif Instruction = MFC then
                    currentState <= Smfc;

                elsif Instruction = MFT then
                    currentState <= Smft;

                elsif Instruction = SYSCALL then
                    currentState <= Ssyscall;

          		else
          			currentState <= Sfetch;

          		end if;

    	    elsif currentState = Swbk then -- Ultimo ciclo de instrues logicas e aritmeticas

                if (Instruction = ADD or Instruction = ADDI or Instruction = SUB or Instruction = SUBI) then

                    if v = '1' then
                        regCAUSE <= std_logic_vector(to_unsigned(12, regCAUSE'length)); -- Throws Overflow
                        newTrapFlag <= '1';
                        currentState <= Sfetch;
                    else
                        regBank(to_integer(unsigned(REGTARGET))) <= regALU;
                        currentState <= Sfetch;
                    end if;

                else

                    regBank(to_integer(unsigned(REGTARGET))) <= regALU;
                    currentState <= Sfetch;

                end if;

    		elsif currentState = Sld then -- Ultimo ciclo de load
		        if nullPointerExceptionFlag = '1' then
		            regCAUSE <= std_logic_vector(to_unsigned(0, regCAUSE'length)); -- Throws NullPointerException
		            newTrapFlag <= '1';
		            currentState <= Sfetch;
		        else
		    		regBank(to_integer(unsigned(REGTARGET))) <= data_in;
		    		currentState <= Sfetch;
		        end if;

    		elsif currentState = Sst then -- Ultimo ciclo de store
         		if nullPointerExceptionFlag = '1' then
          			regCAUSE <= std_logic_vector(to_unsigned(0, regCAUSE'length)); -- Throws NullPointerException
           			newTrapFlag <= '1';
            		currentState <= Sfetch;
          		else
    			 	currentState <= Sfetch;
          		end if;

    		elsif currentState = Sjmp then -- Ultimo ciclo p/ saltos
                regPC <= regALU;
                currentState <= Sfetch;

    		elsif currentState = Ssbrt then -- Ultimo ciclo Subrotina
                regPC <= regALU; -- Atualiza o PC
                regSP <= regSP - 1;
    			currentState <= Sfetch;

    		elsif currentState = Spush then -- Ultimo ciclo p push
                regSP <= regSP - 1;      -- Adicionado na apresentação
    			currentState <= Sfetch;

    		elsif currentState = Srts then -- Ultimo ciclo retorno subronita
                regSP <= regSP + 1;
                regPC <= DataIn; -- Volta o PC da pilha
    			currentState <= Sfetch;

    		elsif currentState = Spop then -- Ultimo ciclo de POP
                regSP <= regSP + 1;
                regBank(to_integer(unsigned(REGTARGET))) <= DataIn; -- Banco de registradores enderaçado <= mem pelo SP
    			currentState <= Sfetch;

    		elsif currentState = Sldsp then -- Ultimo ciclo de load do SP
                regSP <= regALU;
    			currentState <= Sfetch;

    		-- NOVAS INSTRUÇOES T3P2
    		elsif currentState = Spushf then
    			regSP <= regSP - 1;
    			currentState <= Sfetch;

    		elsif currentState = Spopf then
    			regSP <= regSP + 1;
    			regFLAGS <= DataIn(3 downto 0);
    			currentState <= Sfetch;

    		elsif currentState = Srti then
    			regSP <= regSP + 1;
    			regPC <= DataIn;

                if trapCount > 0 then
                    trapCount <= trapCount - 1;  -- Decreases trap stack
                end if;
                                                             
                if trapCount <= 1 then
                    interruptFlag <= '0';  -- Enables I/O interruptions again if trap stack is = 0 (accounting for asynchronous decrease made above)
                end if;

    			currentState <= Sfetch;

    		-- NOVAS INSTRUÇÔES T4P1
    		elsif currentState = Smul then
    			regHIGH <= multiplicador(31 downto 16);
    			regLOW <= multiplicador(15 downto 0);
    			currentState <= Sfetch;

    		elsif currentState = Sdiv then
                if regA /= 0 then
    			    regHIGH <= divisor(31 downto 16);
    			    regLOW <= divisor(15 downto 0);
                else
                    regCAUSE <= std_logic_vector(to_unsigned(15, regCAUSE'length)); -- Throws Division By Zero
                    newTrapFlag <= '1';
                end if;

    			currentState <= Sfetch;

    		elsif currentState = Smfh then
    			regBank(to_integer(unsigned(REGTARGET))) <= regHIGH;
    			currentState <= Sfetch;

    		elsif currentState = Smfl then
    			regBank(to_integer(unsigned(REGTARGET))) <= regLOW;
    			currentState <= Sfetch;

            -- NOVA INSTRUÇÂO T4P2
            elsif currentState = Sldisra then
                regISRA <= regBank(to_integer(unsigned(REGSOURCE1)));
                currentState <= Sfetch;

            -- NOVAS INSTRUÇÕES T5P2
            elsif currentState = Smfc then
                regBank(to_integer(unsigned(REGSOURCE1))) <= regCAUSE;
                currentState <= Sfetch;

            elsif currentState = Smft then
                regBank(to_integer(unsigned(REGSOURCE1))) <= regITR;
                currentState <= Sfetch;

            elsif currentState = Sldtsra then
                regTSRA <= regBank(to_integer(unsigned(REGSOURCE1)));
                currentState <= Sfetch;

            elsif currentState = Ssyscall then
                regCAUSE <= std_logic_vector(to_unsigned(8, regCAUSE'length));
                newTrapFlag <= '1';
                currentState <= Sfetch;

            else
                currentState <= Shalt;
                report "Unexpected currentState" severity error;

    		end if;

    	end if;

    end process;

    -- If SupportsMultiplication is defined as true in R8_PKG, drives "multiplicador" (declared in R8_PKG)
    MulGen: if SupportsMultiplication generate
		multiplicador <= (regA * regB) when Instruction = MUL else (others=>'0');
	end generate MulGen;

	-- If SupportsDivision is defined as true in R8_PKG, drives "divisor" (declared in R8_PKG)
	DivGen: if SupportsDivision generate

		-- Higher order bits <= remainder; Lower order bits <= quotient
		divisor(31 downto 16) <= STD_LOGIC_VECTOR( UNSIGNED(regB) mod UNSIGNED(regA) ) when (Instruction = DIV and regA /= 0) else (others=>'0');
		divisor(15 downto 0) <= STD_LOGIC_VECTOR( SIGNED(regB) / SIGNED(regA) )        when (Instruction = DIV and regA /= 0) else (others=>'0');

	end generate DivGen;

	-- Aux signals needed for operations involving constants (ADDI or SUBI)
	constanteExtended(15 downto 8) <= (others=>CONSTANTE(7));
	constanteExtended(7 downto 0)  <= (CONSTANTE);
	constanteComplement <= ( ( not constanteExtended ) + 1 );

	-- Twos complement of regB
	regBComplement <= ( (not regB) + 1 );

	-- Aux 17 bit signal (needed for flag generation)
	ALUaux <= ( regA(15) & regA ) + ( regB(15) & regB )                                when Instruction = ADD else
			  ( regA(15) & regA ) + ( regBComplement(15) & regBComplement )            when Instruction = SUB else
			  ( regB(15) & regB ) + ( constanteExtended(15) & constanteExtended)       when Instruction = ADDI else
			  ( regB(15) & regB ) + ( constanteComplement(15) & constanteComplement);--when Instruction = SUBI; GENERATES LATCHES IF UNCOMMENTED

	-- Asynchronous ALU (16 bit output). Synchrounously drives regALU on Sula state
    outALU <= ALUaux(15 downto 0)           when Instruction = ADD or Instruction = SUB or Instruction = ADDI or Instruction = SUBI else
              regA and regB                 when Instruction = AAND else
              regA or regB                  when Instruction = OOR else
              regA xor regB                 when Instruction = XXOR else
              regB(15 downto 8) & CONSTANTE when Instruction = LDL else
              CONSTANTE & regB(7 downto 0)  when Instruction = LDH else
              regA + regB                   when Instruction = LD else  -- rt <= PMEM (Rs1 + Rs2)
              regA + regB                   when Instruction = ST else  -- PMEM (Rs1 + Rs2) <- rt
              regA(14 downto 0) & '0'       when Instruction = SL0 else
              regA(14 downto 0) & '1'       when Instruction = SL1 else
              '0' & regA(15 downto 1)       when Instruction = SR0 else
              '1' & regA(15 downto 1)       when Instruction = SR1 else
              not(regA)                     when Instruction = NOT_A else
              regSP + 1                     when Instruction = RTS or Instruction = POP or Instruction = POPF or Instruction = RTI else
              regPC + regA                  when Instruction = JMPR or Instruction = JMPNR or Instruction = JMPZR or Instruction = JMPCR or Instruction = JMPVR else
              regPC + (JMPD_DESLOC(9) & JMPD_DESLOC(9) & JMPD_DESLOC(9) & JMPD_DESLOC(9) & JMPD_DESLOC(9) & JMPD_DESLOC(9) & JMPD_DESLOC)
              								when Instruction = JMPD or Instruction = JMPND or Instruction = JMPZD or Instruction = JMPCD or Instruction = JMPVD else
              regPC + (JSRD_DESLOC(11) & JSRD_DESLOC(11) & JSRD_DESLOC(11) & JSRD_DESLOC(11) & JSRD_DESLOC) when Instruction = JSRD else
              regA                          when Instruction = JSR or Instruction = JSRR else
              regA; -- JMP_A, LDSP

    -- ALU Asynchronous flags
    flagC <= ALUaux(16);
    flagN <= outALU(15);
    flagZ <= '1' when outALU = 0 else '0';
    flagV <= '1' when ( ( ( Instruction = ADD  ) and ( regA(15) = regB(15) ) and ( regA(15) /= outALU(15) ) )  or
                        ( ( Instruction = SUB  ) and ( regA(15) = regBComplement(15) ) and ( regA(15) /= outALU(15) ) )  or
                        ( ( Instruction = ADDI ) and ( regB(15) = constanteExtended(15) ) and ( regB(15) /= outALU(15) ) )  or
                        ( ( Instruction = SUBI ) and ( regB(15) = constanteComplement(15) ) and ( regB(15) /= outALU(15) ) ) ) 
                        else '0';

    -- SINAIS MEMORIA
    Address <= regPC when currentState = Sfetch else
               regALU when currentState = Sld or currentState = Sst or currentState = Spop or currentState = Srts or currentState = Spopf or currentState = Srti else
               regSP; -- Jumps to subroutines and pushes (Spush, Spushf, Sitr, Strap)

    DataOut <= regBank(to_integer(unsigned(REGTARGET))) when currentState = Sst else
                regB when currentState = Spush else
                regPC when currentState = Ssbrt else
				( "000000000000" & regFLAGS ) when currentState = Spushf else
				regPC when currentState = Sitr else
                regPC when currentState = Strap else
                (others=>'0');

    -- '1' when using memory
    CE <= '1' when rst = '0' and ( ( currentState = Sld and ( ( regALU /= 0 and ProgMode = '0' ) or ( ProgMode = '1' ) ) ) or currentState = Ssbrt or currentState = Spush or 
      ( currentState = Sst and ( ( regALU /= 0 and ProgMode = '0' ) or ( ProgMode = '1' ) ) ) or currentState = Sfetch or currentState = Srts or currentState = Spop or currentState = Spopf or currentState = Spushf or currentState = Sitr or currentState = Strap or currentState = Srti ) else '0';

    -- '1' when "read", '0' when "write"
    RW <= '1' when (currentState = Sfetch or currentState = Spop or currentState = Srts or currentState = Sld or currentState = Spopf or currentState = Srti) else '0';

    -- Active when Loading/Storing into/to first memory position in execution mode
    nullPointerExceptionFlag <= '1' when ( (currentState = Sld or currentState = Sst) and regALU = 0 and ProgMode = '0') else '0'; 

end architecture Behavioural;
