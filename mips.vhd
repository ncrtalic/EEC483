------------------------------------------------------------------------------------------------------------
-- Noah Crtalic 2717249
-- Eric Okeafor 
--
-- At this time there are no known problems with
-- the code that we have worked on.
--
-- EEC 483 Project
-- Implementation of MIPS
-- File name   : mips.vhd
--
------------------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;
  
entity mips is
    port (clock_mips	  : in  std_logic;
          reset_mips     : in  std_logic;
          memory_in_mips       : out std_logic_vector (31 downto 0);	-- data to write into memory (sw)
          memory_out_mips      : in  std_logic_vector (31 downto 0); -- data being read from memory (lw)
          memory_address_mips  : out std_logic_vector (31 downto 0);	-- memory address to read/write
          pc_mips              : out std_logic_vector (31 downto 0);	-- instruction address to fetch
          instruction_mips     : in std_logic_vector (31 downto 0);  -- instruction data to execute in the next cycle
          overflow_mips			 : out std_logic;  -- flag for overflow ------- (CORRECTED)
          invalid_mips         : out std_logic;  -- flag for invalid opcode ------- (ADDED)
			 memory_write_mips    : out std_logic);                     -- control signal for data memory read/write
end entity;

architecture rtl of mips is

------ Register file ---
   type register_type is array (0 to 31) of std_logic_vector(31 downto 0);
   signal Registers : register_type := (
   		0 => x"00000000", 
			4 => x"00000004",
			5 => x"00000084",
			6 => x"0000008c",
			7 => x"00000001",
			others => x"00000000");

------ Signal Declaration ------
	 signal wd: std_logic_vector(31 downto 0); 
	 
	 signal alu1, alu2   : std_logic_vector (31 downto 0);   -- ALU input values
    signal ALUOut	: std_logic_vector (31 downto 0);  -- ALU output value
    signal ALUctrl	: std_logic_vector (3 downto 0);  -- ALU output value
	 signal overflow : std_logic;   -- overflow flag
	 signal Zero : std_logic;       -- 1 bit signals for ALU arithmetic operations
	 
	 signal opcode: std_logic_vector(5 downto 0);
	 signal rformat, lw, sw, beq, bne, j: std_logic;
	 signal funct: std_logic_vector(5 downto 0);
	 signal immed: std_logic_vector(15 downto 0);
	 signal extend: std_logic_vector(31 downto 0);
    signal rs, rt, rd, wn: std_logic_vector (4 downto 0);  -- rs, rt, rd, wn
	 signal rd1, rd2: std_logic_vector(31 downto 0); 
	 signal RegDst, RegWrite, MemWrite, ALUSrc, MemtoReg, PCSrc : std_logic;
    signal ALUOpcode	: std_logic_vector (1 downto 0);

    signal pc_next, pc4, jumpAdd, branchaddress		: std_logic_vector (31 downto 0);     -- instruction address to fetch
			
------ Component Declarations ------
component ALU_32
	port(
		A_alu32 : in std_logic_vector(31 downto 0);  	 	-- A input
		B_alu32 : in std_logic_vector(31 downto 0);  	 	-- B input
		ALUctl_alu32 : in std_logic_vector(3 downto 0);  	-- control
		ALUout_alu32 : out std_logic_vector(31 downto 0);  -- result
		overflow_alu32: out std_logic; -- overflow result
		Zero_alu32 : out std_logic);  								-- check if ALUout is zero
end component ;


begin

-- IMPLEMENT MIPS CPU FROM RIGHT TO LEFT
----------------------------------------------------
--synchornous write to registers (Write Register stage)
		-- wn <= 
		-- wd <=
		-- process(CLOCK_mips) begin
		-- 	if (RISING_EDGE(clock_mips)) then
		--			if ...
		--      		Registers (to_integer(unsigned(wn))) <= 
		--			end if;
		--		end if;
		-- end process;

with RegWrite select
         wn <= rd when '1', rt when others;
        with MemtoReg select 
         wd <=memory_out_mips when '1', ALUOut when others;
         process(CLOCK_mips) begin
             if (RISING_EDGE(clock_mips)) then
                    if(RegWrite = '0') then
                              Registers (to_integer(unsigned(wn))) <= wd; 
                    end if;
                end if;
         end process;
			
----------------------------------------------------
--Memory interfacing (MEMORY stage) ----	
  -- memory_in_mips <= ... 
  -- memory_address_mips <= ...
  -- memory_write_mips <= ...
  
  memory_in_mips <= rd2;
  memory_address_mips <= ALUOut;
  memory_write_mips <= MemWrite;
	 
  -- Using the three interfaces, the component "data_memory_64B" is 
  -- either written or read (memory_out_mips)

----------------------------------------------------
--ALU interfacing (ALU execution stage)
	-- alu1 <= ...
	-- alu2 <= ...
	
	-- alu1 <=
	alu1 <= rd1;
	-- alu2 <=
	alu2 <= rd2;

	ALUctrl(3) <= '0'; 							-- Ainvert
	ALUctrl(2) <= (ALUOpcode(1) and (not ALUOpcode(0)) and funct(1)) or (ALUOpcode(0)); -- Binvert
	ALUctrl(1) <= (ALUopcode(1) and (not funct(2))) or (not ALUopcode(1));
	ALUctrl(0) <= ALUOpcode(1) and (funct(3) or ((not funct(1)) and funct(0)));

	
	-- The component "ALU_32" produces output (ALUout, overflow, Zero)
	------ Component Instantiations ------
   ALU_32_1 : ALU_32
      port map ( A_alu32       => alu1,
                 B_alu32       => alu2,
                 ALUctl_alu32  => ALUctrl,
				     ALUout_alu32=>ALUout,
					  overflow_alu32=>overflow,
					  Zero_alu32=>Zero);

----------------------------------------------------
--instruction analysis (instruction decoding stage)
	-- op <= 
	-- funct <= 
	-- rs <= 
	-- rt <= 
	-- rd <= 

	opcode <= instruction_mips(31 downto 26);
	funct <= instruction_mips(5 downto 0);
	rs <= instruction_mips(25 downto 21);
	rt <= instruction_mips(20 downto 16);
	rd <= instruction_mips(15 downto 11);
	
	
	-- Sign Extend
	immed <= instruction_mips(15 downto 0);
	extend(15 downto 0) <= immed;

	with instruction_mips(15) select
	extend(31 downto 16) <= "1111111111111111" when '1',
									"0000000000000000" when others;

									
	-- Control Signals
		-- RegDst <= 
		-- ALUSrc <= 
		-- MemtoReg <= 
		-- RegWrite <= 
		-- MemWrite <= 
		
		Rformat <= ((not(Opcode(5))) and (not(Opcode(4))) and (not(Opcode(3))) and 
			(not(Opcode(2))) and (not(Opcode(1))) and (not(Opcode(0))));
		-- load word 
		lw <= ((Opcode(5)) and (not(Opcode(4))) and (not(Opcode(3))) and 
			(not(Opcode(2))) and (Opcode(1)) and (Opcode(0)));
		-- store word
		sw <= ((Opcode(5)) and (not(Opcode(4))) and (Opcode(3)) and 
			(not(Opcode(2))) and (Opcode(1)) and (Opcode(0)));
		-- the BEQ instruction branches when Zero flag set -- BNE instruction branches when Zero flag clear.
		beq <= ((not(Opcode(5))) and (not(Opcode(4))) and (not(Opcode(3))) and 
			(Opcode(2)) and (not(Opcode(1))) and (not(Opcode(0))));
		-- branch if equal ^
		bne <= ((not(Opcode(5))) and (not(Opcode(4))) and (not(Opcode(3))) and 
			(Not(Opcode(2))) and (Opcode(1)) and (not(Opcode(0))));
		-- branch not equal ^
		j <= ((not(Opcode(5))) and (not(Opcode(4))) and (not(Opcode(3))) and 
			(Not(Opcode(2))) and (Not(Opcode(1))) and (Opcode(0)));
		-- jump^
		
		-- regdst <=
		RegDst <= Rformat;
		-- alusrc <=
		ALUSrc <= lw or sw;
		-- memtoreg <=
		MemtoReg <= lw;
		-- regwrite <=
		RegWrite <= Rformat or lw;
		-- memwrite <=
		MemWrite <= sw;
		
		--Signal to control ALU control signals
		-- ALUOp(1) <= 
		-- ALUOp(0) <= 
		
		ALUOpcode(1) <= Rformat;
		ALUOpcode(0) <= beq;
  
--asynchornous read from registers 
	rd1 <= Registers((to_integer(unsigned(rs))));
	rd2 <= Registers((to_integer(unsigned(rt))));
  
----------------------------------------------------  
-- Instruction memory & PC update (instruction fetch stage) 

  pc_mips <= pc_next;
  pc4 <= pc_next + 4;
  JumpAdd <= pc4 + (instruction_mips(25 downto 0) & "00");
  BranchAddress <= pc4 + (instruction_mips(15 downto 0) & "00");
  --PCSrc <= ZF and beq when '1', ZF and bne when others;
  PCSrc <= (Beq and Zero) or (Bne and not(Zero));
  
  process (CLOCK_mips, reset_mips) 
  begin
     if(reset_mips = '1') then
		  pc_next <= x"00000000";
     elsif(RISING_EDGE(CLOCK_mips)) then
        if (j ='1') then
			 pc_next <= JumpAdd; 
		  else if (PCSrc ='1') then
			 pc_next <= BranchAddress; 
		  else 
			 pc_next <= pc4;
		  end if;             
	  --else 
	  --pc_next <= pc_next;		
		end if;
		end if;
	end process;

	-- The component "inst_memory_128B" produces output (instruction)

end architecture;















