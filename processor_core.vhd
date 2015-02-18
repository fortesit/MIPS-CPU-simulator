library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use IEEE.numeric_std.all;

entity processor_core is
	port (
		clk		:	in std_logic;                       --clock signal
		rst		:	in std_logic;                       --reset signal
		run		:	in std_logic;                       --trigger the miniSPIM to run
		instaddr:	out std_logic_vector(31 downto 0);  --Instruction memory read address
		inst	:	in std_logic_vector(31 downto 0);   --Instruction memory data
		memwen	:	out std_logic;                      --Memory write enable
		memaddr	:	out std_logic_vector(31 downto 0);  --Memory address
		memdw	:	out std_logic_vector(31 downto 0);  --Memory write data
		memdr	:	in std_logic_vector(31 downto 0);   --Memory read data
		fin		:	out std_logic;                      --Indicate execution finish
		PCout	:	out std_logic_vector(31 downto 0);  --PC value when finish
		regaddr	:	in std_logic_vector(4 downto 0);    --Register read address (debug use only)
		regdout	:	out std_logic_vector(31 downto 0)   --register read data (debug user only)
	);
end processor_core;

architecture arch_processor_core of processor_core is
-- Add the register table here
    component regtable IS
		PORT (
            clk		:	in std_logic;                       --clock signal
            rst		:	in std_logic;                       --reset signal
            raddrA	:	in std_logic_vector(4 downto 0);    --register read address 1
            raddrB	:	in std_logic_vector(4 downto 0);    --register read address 2
            wen		:	in std_logic;                       --write enable
            waddr	:	in std_logic_vector(4 downto 0);    --register write address
            din		:	in std_logic_vector(31 downto 0);   --register write data
            doutA	:	out std_logic_vector(31 downto 0);  --register read data 1
            doutB	:	out std_logic_vector(31 downto 0);  --register read data 2
            extaddr	:	in std_logic_vector(4 downto 0);    --External register read address (debug use only)
            extdout	:	out std_logic_vector(31 downto 0)   --External register read datat (debug use only)
        );
	end component;
-- Add signals here

    signal signExtended     : std_logic_vector(31 downto 0);

    signal controlCode  : std_logic_vector(5 downto 0);
    signal writeRegAddr : std_logic_vector(4 downto 0);
    
    signal shiftLeft2   : std_logic_vector(31 downto 0);
    
    signal WBdata       : std_logic_vector(31 downto 0);
    
    --PC related
    signal PCtemp       : std_logic_vector(31 downto 0);
    signal run_temp     : std_logic;
    signal branchResult : std_logic;
    
    --Connected to ALU
    signal registerA        : std_logic_vector(31 downto 0);
    signal registerB        : std_logic_vector(31 downto 0);
    signal aluMultiplexed   : std_logic_vector(31 downto 0);
    signal ALUresult        : std_logic_vector(31 downto 0);
    signal funct        : std_logic_vector(5 downto 0);
    signal ALUcontrol   : std_logic_vector(2 downto 0);
    signal ALUzero      : std_logic;
    
    -- control signal
    signal RegDst  : std_logic;
    signal Jump     : std_logic;
    signal Branch   : std_logic;
    signal MemRead  : std_logic;
    signal MemtoReg : std_logic;
    signal ALUOp    : std_logic_vector(3 downto 0);
    signal MemWrite : std_logic;
    signal ALUSrc   : std_logic;
    signal RegWrite : std_logic;
    
    signal finSignal : std_logic;
    
    
begin
-- Processor Core Behaviour    
    --control unit
    controlCode <= inst(31 downto 26);
    RegDst     <= '1' when controlCode = "000000" else -- 000000 = 0
                  '0';
    Jump        <= '1' when controlCode = "000010" else -- 000010 = 2
                   '0';
    Branch      <= '1' when controlCode = "000100" else -- 000100 = 8
                   '0';
    MemRead     <= '1' when controlCode = "100011" else -- 100011 = 35
                   '0';
    MemToReg    <= '1' when controlCode = "100011" else -- 100011 = 35
                   '0';
    ALUOp       <= "0111" when controlCode = "000000" else --0111 = 7, 000000 = 0, R-type
                   "0010" when controlCode = "001010" else --0010 = 2, 001010 = 10, slti
                   "0011" when controlCode = "001011" else --0011 = 3, 001011 = 11, sltiu
                   "0001" when controlCode = "000100" else --0001 = 1, 000100 = 4, beq
                   "0110" when controlCode = "000110" else --0110 = 6, 000110 = 6, lui
                   "0000" ;
    MemWrite    <= '1' when controlCode = "101011" else -- 101011 = 43
                   '0';
    ALUSrc      <= '0' when controlCode = "000000"      --000000 = 0
                         or controlCode = "000100"      --000100 = 4
                         or controlCode = "000010" else --000010 = 2
                   '1';
    RegWrite    <= '0' when controlCode = "000100"      --000100 = 4
                         or controlCode = "000010"      --000010 = 2
                         or controlCode = "101011" else --101011 = 43
                   '1';
    
    --write register multiplexer
    writeRegAddr <= inst(20 downto 16) when RegDst = '0' else
                    inst(15 downto 11);
    
    --sign-extend
    signExtended <= X"0000" & inst(15 downto 0) when inst(15) = '0' else
                    X"FFFF" & inst(15 downto 0);
    
    --shift-left 2 (before address adder)
    shiftLeft2 <= signExtended(29 downto 0) & "00";    
    
    --ALU multiplexer
    aluMultiplexed <= registerB when ALUSrc = '0' else
                      signExtended;
    
    --ALU control
    funct <= inst(5 downto 0);
    ALUcontrol <= "000" when ALUOp = "0111" and funct = "100000" else --add
                  "001" when ALUOp = "0111" and funct = "100010" else --sub
                  "010" when ALUOp = "0111" and funct = "101010" else --set less signed
                  "011" when ALUOp = "0111" and funct = "101011" else --set less unsigned
                  "100" when ALUOp = "0111" and funct = "100100" else --and
                  "101" when ALUOp = "0111" and funct = "100101" else --or
                  "110" when ALUOp = "0111" and funct = "000110" else --shift left extended value 16
                  "111" when ALUOp = "0111" and funct = "100111" else --nor
                  "000" when ALUOp = "0000" else --addi
                  "010" when ALUOp = "0010" else --slti
                  "011" when ALUOp = "0011" else --sltiu
                  "001" when ALUOp = "0001" else --beq
                  "000" when ALUOp = "0000" else --lw
                  "000" when ALUOp = "0110" else --lui
                  "000" when ALUOp = "0000"; --sw
    
    --Register
    registerTable: regtable
        port map (
            clk		=> clk,
            rst		=> rst,
            raddrA	=> inst(25 downto 21),
            raddrB	=> inst(20 downto 16),
            wen		=> RegWrite,
            waddr	=> writeRegAddr,
            din		=> WBdata,
            doutA	=> registerA,
            doutB	=> registerB,
            extaddr	=> regaddr,
            extdout	=> regdout
        ); 
        
    --PC_update
    branchResult <= ALUzero and Branch;
    process(clk, run, rst)
    begin
      if run = '1' then
        run_temp <= '1';
      end if;
      if rst = '1' then
          PCtemp <= X"00004000";
      elsif run_temp = '1' and clk = '1' and clk'event then 
          if Jump = '1' then 
            PCtemp <= PCtemp(31 downto 28) & inst(25 downto 0) & "00";
          elsif branchResult = '1' then
            PCtemp <= std_logic_vector( unsigned(PCtemp) + 4 + unsigned(shiftLeft2) );
          else
            PCtemp <= std_logic_vector( unsigned(PCtemp) + 4 );
          end if;
      end if;
    end process;
    
    --write_register
    process(memdr, ALUresult)
    begin
      if MemToReg = '1' then
        WBdata <= memdr;
      else
        WBdata <= ALUresult;
      end if;
    end process;
    
    --Processor core output
    memwen  <= MemWrite;
    memdw   <= registerB;
    
    --get instruction from instruction memory
    instaddr <= PCtemp;
    
    --ALU 
    --detect not only ALUControl as it may got cases that input changes but do the same operation
    memaddr <= ALUresult;
    ALUresult <= std_logic_vector(signed(registerA) + signed(aluMultiplexed)) 
                        when ALUControl = "000" else
                 std_logic_vector(signed(registerA) - signed(aluMultiplexed)) 
                        when ALUControl = "001" else
                 X"00000001" 
                        when ALUControl = "010" and signed(registerA) < signed(aluMultiplexed) else
                 X"00000001" 
                        when ALUControl = "011" and unsigned(registerA) < unsigned(aluMultiplexed) else
                 registerA and aluMultiplexed
                        when ALUControl = "100" else
                 registerA or aluMultiplexed
                        when ALUControl = "101" else
                 aluMultiplexed(15 downto 0) & X"0000"
                        when ALUControl = "110" else
                 not registerA
                        when ALUControl = "111" else
                 X"00000000";
    ALUzero <= '1' when ALUControl = "001" and registerA = aluMultiplexed else
               '0';

    fin <= finSignal;
    finSignal <= '1' and run_temp 
                when (controlCode /= "000000" and
                    controlCode /= "001000" and
                    controlCode /= "100011" and
                    controlCode /= "101011" and
                    controlCode /= "001111" and
                    controlCode /= "000100" and
                    controlCode /= "001010" and 
                    controlCode /= "001011" and
                    controlCode /= "000010") or
                    (PCtemp(1 downto 0) /= "00") or
                    (ALUresult (1 downto 0) /= "00" and (MemWrite = '1' or MemtoReg = '1')) or
                    inst(31 downto 0) = X"00000000"
                    else
                '0';

    process (finSignal)
    begin
        if finSignal = '1' then
            PCout <= PCtemp;
        end if;
    end process;
    
end arch_processor_core;