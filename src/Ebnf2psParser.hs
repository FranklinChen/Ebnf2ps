-- parser produced by Happy Version 1.5


module Ebnf2psParser (theEbnfParser, theHappyParser, theYaccParser) where
import AbstractSyntax
import Lexer
import List

data HappyAbsSyn 
	= HappyTerminal Token'
	| HappyErrorToken Int
	| HappyAbsSyn1([Production])
	| HappyAbsSyn4(Production)
	| HappyAbsSyn5([String])
	| HappyAbsSyn6(String)
	| HappyAbsSyn25(())

type HappyReduction = 
	   Int 
	-> (Token')
	-> HappyState (Token') ([HappyAbsSyn] -> [(Token')] -> [Production])
	-> [HappyState (Token') ([HappyAbsSyn] -> [(Token')] -> [Production])] 
	-> [HappyAbsSyn] 
	-> [(Token')] -> [Production]

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124 :: Int -> HappyReduction

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82 :: HappyReduction

action_0 (38) = happyShift action_2
action_0 (39) = happyShift action_3
action_0 (40) = happyShift action_4
action_0 (1) = happyGoto action_1
action_0 _ = happyFail

action_1 (62) = happyAccept
action_1 _ = happyFail

action_2 (41) = happyShift action_15
action_2 (2) = happyGoto action_11
action_2 (3) = happyGoto action_12
action_2 (4) = happyGoto action_13
action_2 (6) = happyGoto action_14
action_2 _ = happyFail

action_3 (49) = happyShift action_10
action_3 (13) = happyGoto action_7
action_3 (25) = happyGoto action_8
action_3 (26) = happyGoto action_9
action_3 _ = happyReduce_51

action_4 (46) = happyShift action_6
action_4 (29) = happyGoto action_5
action_4 _ = happyFail

action_5 _ = happyReduce_3

action_6 (42) = happyShift action_36
action_6 (30) = happyGoto action_34
action_6 (32) = happyGoto action_35
action_6 _ = happyFail

action_7 _ = happyReduce_2

action_8 (47) = happyShift action_33
action_8 (21) = happyGoto action_31
action_8 (22) = happyGoto action_32
action_8 _ = happyFail

action_9 _ = happyReduce_50

action_10 (41) = happyShift action_22
action_10 (43) = happyShift action_23
action_10 (44) = happyShift action_24
action_10 (45) = happyShift action_25
action_10 (46) = happyShift action_26
action_10 (47) = happyShift action_27
action_10 (48) = happyShift action_28
action_10 (49) = happyShift action_10
action_10 (59) = happyShift action_29
action_10 (60) = happyShift action_30
action_10 (26) = happyGoto action_19
action_10 (27) = happyGoto action_20
action_10 (28) = happyGoto action_21
action_10 _ = happyFail

action_11 _ = happyReduce_1

action_12 (41) = happyShift action_15
action_12 (4) = happyGoto action_18
action_12 (6) = happyGoto action_14
action_12 _ = happyReduce_4

action_13 _ = happyReduce_5

action_14 (59) = happyShift action_17
action_14 (5) = happyGoto action_16
action_14 _ = happyReduce_9

action_15 _ = happyReduce_10

action_16 (52) = happyShift action_48
action_16 _ = happyFail

action_17 _ = happyReduce_8

action_18 _ = happyReduce_6

action_19 _ = happyReduce_57

action_20 (50) = happyShift action_47
action_20 _ = happyFail

action_21 (41) = happyShift action_22
action_21 (43) = happyShift action_23
action_21 (44) = happyShift action_24
action_21 (45) = happyShift action_25
action_21 (46) = happyShift action_26
action_21 (47) = happyShift action_27
action_21 (48) = happyShift action_28
action_21 (49) = happyShift action_10
action_21 (59) = happyShift action_29
action_21 (60) = happyShift action_30
action_21 (26) = happyGoto action_19
action_21 (27) = happyGoto action_46
action_21 (28) = happyGoto action_21
action_21 _ = happyReduce_54

action_22 _ = happyReduce_56

action_23 _ = happyReduce_60

action_24 _ = happyReduce_59

action_25 _ = happyReduce_61

action_26 _ = happyReduce_63

action_27 _ = happyReduce_64

action_28 _ = happyReduce_62

action_29 _ = happyReduce_55

action_30 _ = happyReduce_58

action_31 (46) = happyShift action_45
action_31 _ = happyFail

action_32 (47) = happyShift action_33
action_32 (21) = happyGoto action_44
action_32 (22) = happyGoto action_32
action_32 _ = happyReduce_41

action_33 (41) = happyShift action_43
action_33 _ = happyFail

action_34 (42) = happyShift action_36
action_34 (46) = happyShift action_41
action_34 (48) = happyShift action_42
action_34 (31) = happyGoto action_38
action_34 (32) = happyGoto action_39
action_34 (37) = happyGoto action_40
action_34 _ = happyReduce_82

action_35 _ = happyReduce_66

action_36 (44) = happyShift action_37
action_36 _ = happyFail

action_37 (33) = happyGoto action_69
action_37 _ = happyReduce_73

action_38 _ = happyReduce_67

action_39 _ = happyReduce_68

action_40 _ = happyReduce_65

action_41 _ = happyReduce_81

action_42 (33) = happyGoto action_68
action_42 _ = happyReduce_73

action_43 (41) = happyShift action_66
action_43 (49) = happyShift action_10
action_43 (59) = happyShift action_67
action_43 (23) = happyGoto action_63
action_43 (24) = happyGoto action_64
action_43 (26) = happyGoto action_65
action_43 _ = happyReduce_49

action_44 _ = happyReduce_40

action_45 (41) = happyShift action_62
action_45 (14) = happyGoto action_60
action_45 (15) = happyGoto action_61
action_45 _ = happyFail

action_46 _ = happyReduce_53

action_47 _ = happyReduce_52

action_48 (41) = happyShift action_55
action_48 (49) = happyShift action_56
action_48 (54) = happyShift action_57
action_48 (56) = happyShift action_58
action_48 (59) = happyShift action_59
action_48 (7) = happyGoto action_49
action_48 (8) = happyGoto action_50
action_48 (9) = happyGoto action_51
action_48 (10) = happyGoto action_52
action_48 (11) = happyGoto action_53
action_48 (12) = happyGoto action_54
action_48 _ = happyFail

action_49 (43) = happyShift action_92
action_49 _ = happyFail

action_50 (48) = happyShift action_91
action_50 _ = happyReduce_11

action_51 _ = happyReduce_12

action_52 _ = happyReduce_14

action_53 (41) = happyShift action_55
action_53 (49) = happyShift action_56
action_53 (54) = happyShift action_57
action_53 (56) = happyShift action_58
action_53 (59) = happyShift action_59
action_53 (10) = happyGoto action_90
action_53 (11) = happyGoto action_53
action_53 (12) = happyGoto action_54
action_53 _ = happyReduce_15

action_54 (53) = happyShift action_88
action_54 (58) = happyShift action_89
action_54 _ = happyReduce_17

action_55 _ = happyReduce_20

action_56 (41) = happyShift action_55
action_56 (49) = happyShift action_56
action_56 (54) = happyShift action_57
action_56 (56) = happyShift action_58
action_56 (59) = happyShift action_59
action_56 (7) = happyGoto action_87
action_56 (8) = happyGoto action_50
action_56 (9) = happyGoto action_51
action_56 (10) = happyGoto action_52
action_56 (11) = happyGoto action_53
action_56 (12) = happyGoto action_54
action_56 _ = happyFail

action_57 (41) = happyShift action_55
action_57 (49) = happyShift action_56
action_57 (54) = happyShift action_57
action_57 (56) = happyShift action_58
action_57 (59) = happyShift action_59
action_57 (7) = happyGoto action_86
action_57 (8) = happyGoto action_50
action_57 (9) = happyGoto action_51
action_57 (10) = happyGoto action_52
action_57 (11) = happyGoto action_53
action_57 (12) = happyGoto action_54
action_57 _ = happyFail

action_58 (41) = happyShift action_55
action_58 (49) = happyShift action_56
action_58 (54) = happyShift action_57
action_58 (56) = happyShift action_58
action_58 (59) = happyShift action_59
action_58 (7) = happyGoto action_85
action_58 (8) = happyGoto action_50
action_58 (9) = happyGoto action_51
action_58 (10) = happyGoto action_52
action_58 (11) = happyGoto action_53
action_58 (12) = happyGoto action_54
action_58 _ = happyFail

action_59 _ = happyReduce_21

action_60 (49) = happyShift action_10
action_60 (25) = happyGoto action_84
action_60 (26) = happyGoto action_9
action_60 _ = happyReduce_51

action_61 (41) = happyShift action_62
action_61 (14) = happyGoto action_83
action_61 (15) = happyGoto action_61
action_61 _ = happyReduce_27

action_62 (44) = happyShift action_81
action_62 (45) = happyShift action_82
action_62 _ = happyFail

action_63 _ = happyReduce_42

action_64 _ = happyReduce_46

action_65 (49) = happyShift action_10
action_65 (26) = happyGoto action_80
action_65 _ = happyReduce_43

action_66 (49) = happyShift action_10
action_66 (26) = happyGoto action_79
action_66 _ = happyReduce_44

action_67 (49) = happyShift action_10
action_67 (26) = happyGoto action_78
action_67 _ = happyFail

action_68 (41) = happyShift action_73
action_68 (47) = happyShift action_74
action_68 (49) = happyShift action_75
action_68 (59) = happyShift action_76
action_68 (34) = happyGoto action_77
action_68 (35) = happyGoto action_71
action_68 (36) = happyGoto action_72
action_68 _ = happyReduce_77

action_69 (41) = happyShift action_73
action_69 (47) = happyShift action_74
action_69 (49) = happyShift action_75
action_69 (59) = happyShift action_76
action_69 (34) = happyGoto action_70
action_69 (35) = happyGoto action_71
action_69 (36) = happyGoto action_72
action_69 _ = happyReduce_77

action_70 (43) = happyShift action_109
action_70 _ = happyReduce_70

action_71 _ = happyReduce_71

action_72 _ = happyReduce_72

action_73 _ = happyReduce_78

action_74 (41) = happyShift action_111
action_74 _ = happyFail

action_75 (50) = happyShift action_110
action_75 _ = happyFail

action_76 _ = happyReduce_79

action_77 (43) = happyShift action_109
action_77 _ = happyReduce_69

action_78 (41) = happyShift action_107
action_78 (59) = happyShift action_67
action_78 (24) = happyGoto action_108
action_78 _ = happyReduce_49

action_79 (41) = happyShift action_107
action_79 (59) = happyShift action_67
action_79 (24) = happyGoto action_106
action_79 _ = happyReduce_49

action_80 (49) = happyShift action_10
action_80 (26) = happyGoto action_105
action_80 _ = happyFail

action_81 (41) = happyShift action_103
action_81 (59) = happyShift action_104
action_81 (17) = happyGoto action_99
action_81 (18) = happyGoto action_100
action_81 (19) = happyGoto action_101
action_81 (20) = happyGoto action_102
action_81 _ = happyReduce_37

action_82 (49) = happyShift action_10
action_82 (26) = happyGoto action_98
action_82 _ = happyFail

action_83 _ = happyReduce_26

action_84 _ = happyReduce_25

action_85 (57) = happyShift action_97
action_85 _ = happyFail

action_86 (55) = happyShift action_96
action_86 _ = happyFail

action_87 (50) = happyShift action_95
action_87 _ = happyFail

action_88 _ = happyReduce_19

action_89 (41) = happyShift action_55
action_89 (49) = happyShift action_56
action_89 (54) = happyShift action_57
action_89 (56) = happyShift action_58
action_89 (59) = happyShift action_59
action_89 (12) = happyGoto action_94
action_89 _ = happyFail

action_90 _ = happyReduce_16

action_91 (41) = happyShift action_55
action_91 (49) = happyShift action_56
action_91 (54) = happyShift action_57
action_91 (56) = happyShift action_58
action_91 (59) = happyShift action_59
action_91 (9) = happyGoto action_93
action_91 (10) = happyGoto action_52
action_91 (11) = happyGoto action_53
action_91 (12) = happyGoto action_54
action_91 _ = happyFail

action_92 _ = happyReduce_7

action_93 _ = happyReduce_13

action_94 _ = happyReduce_18

action_95 _ = happyReduce_24

action_96 _ = happyReduce_23

action_97 _ = happyReduce_22

action_98 (41) = happyShift action_117
action_98 (44) = happyShift action_118
action_98 (16) = happyGoto action_116
action_98 _ = happyFail

action_99 _ = happyReduce_29

action_100 (48) = happyShift action_115
action_100 _ = happyReduce_33

action_101 (49) = happyShift action_10
action_101 (26) = happyGoto action_114
action_101 _ = happyFail

action_102 (41) = happyShift action_103
action_102 (59) = happyShift action_104
action_102 (19) = happyGoto action_113
action_102 (20) = happyGoto action_102
action_102 _ = happyReduce_37

action_103 _ = happyReduce_39

action_104 _ = happyReduce_38

action_105 _ = happyReduce_45

action_106 _ = happyReduce_47

action_107 (49) = happyShift action_10
action_107 (26) = happyGoto action_79
action_107 _ = happyFail

action_108 _ = happyReduce_48

action_109 _ = happyReduce_76

action_110 _ = happyReduce_80

action_111 (41) = happyShift action_73
action_111 (59) = happyShift action_76
action_111 (35) = happyGoto action_112
action_111 _ = happyFail

action_112 (49) = happyShift action_75
action_112 (36) = happyGoto action_123
action_112 _ = happyReduce_74

action_113 _ = happyReduce_36

action_114 (43) = happyShift action_122
action_114 _ = happyReduce_35

action_115 (41) = happyShift action_103
action_115 (59) = happyShift action_104
action_115 (17) = happyGoto action_121
action_115 (18) = happyGoto action_100
action_115 (19) = happyGoto action_101
action_115 (20) = happyGoto action_102
action_115 _ = happyReduce_37

action_116 _ = happyReduce_28

action_117 (44) = happyShift action_120
action_117 _ = happyFail

action_118 (41) = happyShift action_103
action_118 (59) = happyShift action_104
action_118 (17) = happyGoto action_119
action_118 (18) = happyGoto action_100
action_118 (19) = happyGoto action_101
action_118 (20) = happyGoto action_102
action_118 _ = happyReduce_37

action_119 _ = happyReduce_31

action_120 (41) = happyShift action_103
action_120 (59) = happyShift action_104
action_120 (17) = happyGoto action_124
action_120 (18) = happyGoto action_100
action_120 (19) = happyGoto action_101
action_120 (20) = happyGoto action_102
action_120 _ = happyReduce_37

action_121 _ = happyReduce_32

action_122 _ = happyReduce_34

action_123 _ = happyReduce_75

action_124 _ = happyReduce_30

happyReduce_1 = happySpecReduce_2 1 reduction where {
  reduction
	(HappyAbsSyn1  happy_var_2)
	_
	 =  HappyAbsSyn1
		 (happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_2 = happySpecReduce_2 1 reduction where {
  reduction
	(HappyAbsSyn1  happy_var_2)
	_
	 =  HappyAbsSyn1
		 (happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_3 = happySpecReduce_2 1 reduction where {
  reduction
	(HappyAbsSyn1  happy_var_2)
	_
	 =  HappyAbsSyn1
		 (happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_4 = happySpecReduce_1 2 reduction where {
  reduction
	(HappyAbsSyn1  happy_var_1)
	 =  HappyAbsSyn1
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_5 = happySpecReduce_1 3 reduction where {
  reduction
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn1
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_6 = happySpecReduce_2 3 reduction where {
  reduction
	(HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn1  happy_var_1)
	 =  HappyAbsSyn1
		 (happy_var_1 ++ [happy_var_2]);
  reduction _ _  = notHappyAtAll }

happyReduce_7 = happyReduce 5 4 reduction where {
  reduction
	(_ :
	(HappyAbsSyn4  happy_var_4) :
	_ :
	(HappyAbsSyn5  happy_var_2) :
	(HappyAbsSyn6  happy_var_1) :
	happyRest)
	 = HappyAbsSyn4
		 (ProdProduction happy_var_1 happy_var_2 happy_var_4) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_8 = happySpecReduce_1 5 reduction where {
  reduction
	(HappyTerminal (String' happy_var_1))
	 =  HappyAbsSyn5
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_9 = happySpecReduce_0 5 reduction where {
  reduction
	 =  HappyAbsSyn5
		 ([])}

happyReduce_10 = happySpecReduce_1 6 reduction where {
  reduction
	(HappyTerminal (Ident' happy_var_1))
	 =  HappyAbsSyn6
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_11 = happySpecReduce_1 7 reduction where {
  reduction
	(HappyAbsSyn1  happy_var_1)
	 =  HappyAbsSyn4
		 (ProdTerm happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_12 = happySpecReduce_1 8 reduction where {
  reduction
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn1
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_13 = happySpecReduce_3 8 reduction where {
  reduction
	(HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn1  happy_var_1)
	 =  HappyAbsSyn1
		 (happy_var_1 ++ [happy_var_3]);
  reduction _ _ _  = notHappyAtAll }

happyReduce_14 = happySpecReduce_1 9 reduction where {
  reduction
	(HappyAbsSyn1  happy_var_1)
	 =  HappyAbsSyn4
		 (ProdFactor happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_15 = happySpecReduce_1 10 reduction where {
  reduction
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn1
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_16 = happySpecReduce_2 10 reduction where {
  reduction
	(HappyAbsSyn1  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn1
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_17 = happySpecReduce_1 11 reduction where {
  reduction
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_18 = happySpecReduce_3 11 reduction where {
  reduction
	(HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (ProdRepeatWithAtom happy_var_1 happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_19 = happySpecReduce_2 11 reduction where {
  reduction
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (ProdRepeat1 happy_var_1);
  reduction _ _  = notHappyAtAll }

happyReduce_20 = happySpecReduce_1 12 reduction where {
  reduction
	(HappyTerminal (Ident' happy_var_1))
	 =  HappyAbsSyn4
		 (ProdNonterminal happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_21 = happySpecReduce_1 12 reduction where {
  reduction
	(HappyTerminal (String' happy_var_1))
	 =  HappyAbsSyn4
		 (ProdTerminal happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_22 = happySpecReduce_3 12 reduction where {
  reduction
	_
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_23 = happySpecReduce_3 12 reduction where {
  reduction
	_
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (ProdOption happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_24 = happySpecReduce_3 12 reduction where {
  reduction
	_
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (ProdRepeat happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_25 = happyReduce 5 13 reduction where {
  reduction
	(_ :
	(HappyAbsSyn1  happy_var_4) :
	_ :
	(HappyAbsSyn5  happy_var_2) :
	_ :
	happyRest)
	 = HappyAbsSyn1
		 (happyPrepare happy_var_2 happy_var_4) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_26 = happySpecReduce_2 14 reduction where {
  reduction
	(HappyAbsSyn1  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn1
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_27 = happySpecReduce_1 14 reduction where {
  reduction
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn1
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_28 = happyReduce 4 15 reduction where {
  reduction
	((HappyAbsSyn4  happy_var_4) :
	_ :
	_ :
	(HappyTerminal (Ident' happy_var_1)) :
	happyRest)
	 = HappyAbsSyn4
		 (ProdProduction happy_var_1 [] happy_var_4) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_29 = happySpecReduce_3 15 reduction where {
  reduction
	(HappyAbsSyn1  happy_var_3)
	_
	(HappyTerminal (Ident' happy_var_1))
	 =  HappyAbsSyn4
		 (ProdProduction happy_var_1 [] (ProdTerm happy_var_3));
  reduction _ _ _  = notHappyAtAll }

happyReduce_30 = happySpecReduce_3 16 reduction where {
  reduction
	(HappyAbsSyn1  happy_var_3)
	_
	_
	 =  HappyAbsSyn4
		 (ProdTerm happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_31 = happySpecReduce_2 16 reduction where {
  reduction
	(HappyAbsSyn1  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (ProdTerm happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_32 = happySpecReduce_3 17 reduction where {
  reduction
	(HappyAbsSyn1  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn1
		 (happy_var_1 : happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_33 = happySpecReduce_1 17 reduction where {
  reduction
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn1
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_34 = happySpecReduce_3 18 reduction where {
  reduction
	_
	_
	(HappyAbsSyn1  happy_var_1)
	 =  HappyAbsSyn4
		 (ProdFactor happy_var_1);
  reduction _ _ _  = notHappyAtAll }

happyReduce_35 = happySpecReduce_2 18 reduction where {
  reduction
	_
	(HappyAbsSyn1  happy_var_1)
	 =  HappyAbsSyn4
		 (ProdFactor happy_var_1);
  reduction _ _  = notHappyAtAll }

happyReduce_36 = happySpecReduce_2 19 reduction where {
  reduction
	(HappyAbsSyn1  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn1
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_37 = happySpecReduce_0 19 reduction where {
  reduction
	 =  HappyAbsSyn1
		 ([])}

happyReduce_38 = happySpecReduce_1 20 reduction where {
  reduction
	(HappyTerminal (String' happy_var_1))
	 =  HappyAbsSyn4
		 (ProdTerminal    happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_39 = happySpecReduce_1 20 reduction where {
  reduction
	(HappyTerminal (Ident' happy_var_1))
	 =  HappyAbsSyn4
		 (ProdNonterminal happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_40 = happySpecReduce_2 21 reduction where {
  reduction
	(HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 ++ happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_41 = happySpecReduce_1 21 reduction where {
  reduction
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_42 = happySpecReduce_3 22 reduction where {
  reduction
	(HappyAbsSyn5  happy_var_3)
	_
	_
	 =  HappyAbsSyn5
		 (happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_43 = happySpecReduce_1 23 reduction where {
  reduction
	_
	 =  HappyAbsSyn5
		 ([])}

happyReduce_44 = happySpecReduce_1 23 reduction where {
  reduction
	_
	 =  HappyAbsSyn5
		 ([])}

happyReduce_45 = happySpecReduce_3 23 reduction where {
  reduction
	_
	_
	_
	 =  HappyAbsSyn5
		 ([])}

happyReduce_46 = happySpecReduce_1 23 reduction where {
  reduction
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_47 = happySpecReduce_3 24 reduction where {
  reduction
	(HappyAbsSyn5  happy_var_3)
	_
	(HappyTerminal (Ident' happy_var_1))
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_48 = happySpecReduce_3 24 reduction where {
  reduction
	(HappyAbsSyn5  happy_var_3)
	_
	_
	 =  HappyAbsSyn5
		 (happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_49 = happySpecReduce_0 24 reduction where {
  reduction
	 =  HappyAbsSyn5
		 ([])}

happyReduce_50 = happySpecReduce_1 25 reduction where {
  reduction
	_
	 =  HappyAbsSyn25
		 (())}

happyReduce_51 = happySpecReduce_0 25 reduction where {
  reduction
	 =  HappyAbsSyn25
		 (())}

happyReduce_52 = happySpecReduce_3 26 reduction where {
  reduction
	_
	_
	_
	 =  HappyAbsSyn25
		 (())}

happyReduce_53 = happySpecReduce_2 27 reduction where {
  reduction
	_
	_
	 =  HappyAbsSyn25
		 (())}

happyReduce_54 = happySpecReduce_1 27 reduction where {
  reduction
	_
	 =  HappyAbsSyn25
		 (())}

happyReduce_55 = happySpecReduce_1 28 reduction where {
  reduction
	_
	 =  HappyAbsSyn25
		 (())}

happyReduce_56 = happySpecReduce_1 28 reduction where {
  reduction
	_
	 =  HappyAbsSyn25
		 (())}

happyReduce_57 = happySpecReduce_1 28 reduction where {
  reduction
	_
	 =  HappyAbsSyn25
		 (())}

happyReduce_58 = happySpecReduce_1 28 reduction where {
  reduction
	_
	 =  HappyAbsSyn25
		 (())}

happyReduce_59 = happySpecReduce_1 28 reduction where {
  reduction
	_
	 =  HappyAbsSyn25
		 (())}

happyReduce_60 = happySpecReduce_1 28 reduction where {
  reduction
	_
	 =  HappyAbsSyn25
		 (())}

happyReduce_61 = happySpecReduce_1 28 reduction where {
  reduction
	_
	 =  HappyAbsSyn25
		 (())}

happyReduce_62 = happySpecReduce_1 28 reduction where {
  reduction
	_
	 =  HappyAbsSyn25
		 (())}

happyReduce_63 = happySpecReduce_1 28 reduction where {
  reduction
	_
	 =  HappyAbsSyn25
		 (())}

happyReduce_64 = happySpecReduce_1 28 reduction where {
  reduction
	_
	 =  HappyAbsSyn25
		 (())}

happyReduce_65 = happySpecReduce_3 29 reduction where {
  reduction
	_
	(HappyAbsSyn1  happy_var_2)
	_
	 =  HappyAbsSyn1
		 (yaccPrepare happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_66 = happySpecReduce_1 30 reduction where {
  reduction
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn1
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_67 = happySpecReduce_2 30 reduction where {
  reduction
	(HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn1  happy_var_1)
	 =  HappyAbsSyn1
		 (happy_var_1 ++ [happy_var_2]);
  reduction _ _  = notHappyAtAll }

happyReduce_68 = happySpecReduce_1 31 reduction where {
  reduction
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_69 = happySpecReduce_3 31 reduction where {
  reduction
	_
	(HappyAbsSyn1  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (ProdFactor happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_70 = happyReduce 4 32 reduction where {
  reduction
	(_ :
	(HappyAbsSyn1  happy_var_3) :
	_ :
	(HappyTerminal (CIdent' happy_var_1)) :
	happyRest)
	 = HappyAbsSyn4
		 (ProdProduction happy_var_1 [] (ProdTerm [ProdFactor happy_var_3])) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_71 = happySpecReduce_2 33 reduction where {
  reduction
	(HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn1  happy_var_1)
	 =  HappyAbsSyn1
		 (happy_var_1 ++ [happy_var_2]);
  reduction _ _  = notHappyAtAll }

happyReduce_72 = happySpecReduce_2 33 reduction where {
  reduction
	_
	(HappyAbsSyn1  happy_var_1)
	 =  HappyAbsSyn1
		 (happy_var_1);
  reduction _ _  = notHappyAtAll }

happyReduce_73 = happySpecReduce_0 33 reduction where {
  reduction
	 =  HappyAbsSyn1
		 ([])}

happyReduce_74 = happySpecReduce_3 34 reduction where {
  reduction
	_
	_
	_
	 =  HappyAbsSyn25
		 (())}

happyReduce_75 = happyReduce 4 34 reduction where {
  reduction
	(_ :
	_ :
	_ :
	_ :
	happyRest)
	 = HappyAbsSyn25
		 (()) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_76 = happySpecReduce_2 34 reduction where {
  reduction
	_
	_
	 =  HappyAbsSyn25
		 (())}

happyReduce_77 = happySpecReduce_0 34 reduction where {
  reduction
	 =  HappyAbsSyn25
		 (())}

happyReduce_78 = happySpecReduce_1 35 reduction where {
  reduction
	(HappyTerminal (Ident' happy_var_1))
	 =  HappyAbsSyn4
		 (ProdTerminal happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_79 = happySpecReduce_1 35 reduction where {
  reduction
	(HappyTerminal (String' happy_var_1))
	 =  HappyAbsSyn4
		 (ProdTerminal happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_80 = happySpecReduce_2 36 reduction where {
  reduction
	_
	_
	 =  HappyAbsSyn25
		 (())}

happyReduce_81 = happySpecReduce_1 37 reduction where {
  reduction
	_
	 =  HappyAbsSyn25
		 (())}

happyReduce_82 = happySpecReduce_0 37 reduction where {
  reduction
	 =  HappyAbsSyn25
		 (())}

happyNewToken action sts stk [] =
	action 62 62 (error "reading EOF!") (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	EbnfInput -> cont 38;
	HappyInput -> cont 39;
	YaccInput -> cont 40;
	Ident' happy_dollar_dollar -> cont 41;
	CIdent' happy_dollar_dollar -> cont 42;
	SemiColon -> cont 43;
	Colon -> cont 44;
	DoubleColon -> cont 45;
	DoublePercent -> cont 46;
	Percent -> cont 47;
	Bar -> cont 48;
	OpenBrace -> cont 49;
	ClosingBrace -> cont 50;
	Dot -> cont 51;
	Equal -> cont 52;
	Plus -> cont 53;
	OpenBrack -> cont 54;
	ClosingBrack -> cont 55;
	OpenParen -> cont 56;
	ClosingParen -> cont 57;
	Slash -> cont 58;
	String' happy_dollar_dollar -> cont 59;
	Symbol' happy_dollar_dollar -> cont 60;
	Number' _ -> cont 61;
	}

happyThen = \m k -> k m
happyReturn = \a tks -> a
ebnf2psParser = happyParse











happyError :: [Token'] -> a
happyError ts = error ("Parse error in " ++
                         case ts of
                         [] -> " at EOF\n"
                         _  ->  "before\n" ++ showList (take 20 (dropWhile (==Newline) ts)) [] ++ "\n")



unlit :: String -> String
unlit = unlines . map p . lines
    where p ('>':' ':cs)  = cs
          p ('>':'\t':cs) = cs
          p _             = [] 



yaccpreprocessor :: String -> String 
yaccpreprocessor "" = ""
yaccpreprocessor ('%':'%':cs) = '%':'%': yaccRules cs
yaccpreprocessor ('\n':cs)    = '\n':yaccpreprocessor cs
yaccpreprocessor (_:cs)       = yaccpreprocessor cs   

yaccRules :: String -> String 
yaccRules "" = ""
yaccRules ('/':'*':cs) = yaccRules (dropCComment 0 cs)
yaccRules ('%':'{':cs) = yaccRules  (dropCSyntax cs)
yaccRules ('%':'%':cs) = "%%" 
yaccRules ('\'':'{':'\'':cs) = '\'':'{':'\'': yaccRules cs
yaccRules ('{':cs)     = '{':yaccRules (dropActions 0 cs)
yaccRules (c:cs)       = c:yaccRules cs

dropCSyntax :: String -> String
dropCSyntax "" = ""
dropCSyntax ('%':'}':cs) = cs
dropCSyntax ('\n':cs) = '\n':dropCSyntax cs
dropCSyntax (c:cs) = dropCSyntax cs

dropCComment :: Int -> String -> String
dropCComment _ "" = ""
dropCComment n ('/':'*':cs) = dropCComment (n+1) cs
dropCComment n ('\n':cs) = '\n':dropCComment n cs
dropCComment n ('*':'/':cs) 
             | n == 0 = cs
             | otherwise = dropCComment (n-1) cs
dropCComment n (c:cs) = dropCComment n cs


dropActions :: Int -> String -> String 
dropActions _ "" = ""
dropActions n ('"':cs) = dropActions n css where (_,css) = lexString cs
dropActions n ('\'':'{':'\'':cs) = dropActions n cs
dropActions n ('\'':'}':'\'':cs) = dropActions n cs
dropActions n ('{':cs) = dropActions (n+1) cs
dropActions n ('\n':cs) = '\n':dropActions n cs
dropActions n ('}':cs) 
            | n == 0 = '}':cs
            | otherwise = dropActions (n-1) cs
dropActions n (c:cs) = dropActions n cs                 




data Token'
       = EbnfInput
       | HappyInput
       | YaccInput
       | Newline
       | Ident'  String 
       | CIdent' String
       | Symbol' String
       | String' String
       | Number' String
       | Percent
       | DoublePercent
       | OpenBrace  
       | ClosingBrace
       | Bar 
       | SemiColon 
       | DoubleColon 
       | Colon
       | OpenBrack 
       | ClosingBrack
       | OpenParen 
       | ClosingParen
       | Dot 
       | Equal 
       | Plus 
       | Slash
  deriving Eq


instance Show Token' where 
 showsPrec n (Ident' s) = showChar '[' . showString s . showString "] "
 showsPrec n (CIdent' s) = showChar '/' . showString s . showString "/"
 showsPrec n (Symbol' "\n") = showChar '\n'
 showsPrec n (Symbol' s) = showChar '<' . showString s . showString "> "
 showsPrec n (String' s) = showChar '"' . showString s . showString "\" "     
 showsPrec n (Number' s) = showChar ' ' . showString s . showChar ' ' 
 showsPrec n Percent = showString "%"
 showsPrec n DoublePercent = showString "%% "
 showsPrec n OpenBrace = showString "{ "
 showsPrec n ClosingBrace = showString "} "
 showsPrec n OpenBrack = showString "[ "
 showsPrec n ClosingBrack = showString "] "
 showsPrec n OpenParen = showString "( "
 showsPrec n ClosingParen = showString ") "
 showsPrec n Bar = showString "| "
 showsPrec n SemiColon = showString "; "
 showsPrec n DoubleColon = showString ":: "
 showsPrec n Colon = showString ": "
 showsPrec n Dot = showString ". "
 showsPrec n Equal = showString "= "
 showsPrec n Plus = showString "+ "
 showsPrec n Slash = showString "/ "
 showsPrec n Newline = showString "\n"
 showsPrec n YaccInput = showString "\n>>YACC input format<<\n"
 showsPrec n EbnfInput  = showString "\n>>EBNF input format<<\n" 
 showsPrec n HappyInput = showString "\n>>HAPPY input format<<\n" 
 showList [] = id
 showList (x:xs) = shows x . showList xs




ebnf_postlexer = \s -> EbnfInput : foldr f [] s
  where f (Symbol "\n") = id --Newline
        f (Symbol "=")  = (Equal:)
        f (Symbol ".")  = (Dot:)
        f (Symbol "|")  = (Bar:)
        f (Symbol "/")  = (Slash:)
        f (Symbol "+")  = (Plus:)
        f (Symbol "(")  = (OpenParen:)
        f (Symbol "[")  = (OpenBrack:)
        f (Symbol "{")  = (OpenBrace:)
        f (Symbol ")")  = (ClosingParen:)
        f (Symbol "]")  = (ClosingBrack:)
        f (Symbol "}")  = (ClosingBrace:)
        f (Symbol ";")  = (SemiColon:)
        f (Symbol s)    = (Symbol' s:)
        f (Ident s)     = (Ident'  s:)
        f (String s)    = (String' s:)
        f (Number n)    = (Symbol' n:)



happy_postlexer = \s -> HappyInput : foldr f [] s
  where f (Symbol "\n") = id --Newline
        f (Symbol "%%") = (DoublePercent:)
        f (Symbol "%")  = (Percent:)
        f (Symbol "{")  = (OpenBrace:)
        f (Symbol "}")  = (ClosingBrace:)
        f (Symbol "::") = (DoubleColon:)
        f (Symbol ":")  = (Colon:)
        f (Symbol ";")  = (SemiColon:)
        f (Symbol "|")  = (Bar:)
        f (Symbol s)    = (Symbol' s:)
        f (Ident s)     = (Ident'  s:)
        f (String s)    = (String' s:)
        f (Number n)    = (Symbol' n:)



yacc_postlexer s = YaccInput : f s
  where toSkip [] = False
        toSkip (Symbol "\n":cs') = toSkip cs'
        toSkip (Symbol ":":_) = True
        toSkip (c:_) = False
        f [] = []
        f (Symbol "\n":cs) = f cs		-- Newline
        f (Symbol "%":cs)  = Percent : f cs
        f (Symbol "%%":cs) = DoublePercent : f cs
        f (Symbol "|":cs)  = Bar : f cs
        f (Symbol "{":cs)  = OpenBrace : f cs
        f (Symbol "}":cs)  = ClosingBrace : f cs
        f (Symbol ";":cs)  = SemiColon : f cs
        f (Symbol ":":cs)  = Colon : f cs
        f (Symbol c :cs) = (Symbol' c): f cs
        f (String c :cs) = (String' c): f cs
        f (Number c :cs) = (Number' c): f cs
        f (Ident c :cs) | toSkip cs = (CIdent' c): f cs
                        | otherwise = (Ident' c): f cs


happyPrepare terminalsyms = map (happyPrepare' terminalsyms)
happyPrepare' ts (ProdProduction s1 s2 prod)  = ProdProduction s1 s2 (happyPrepare' ts prod)
happyPrepare' ts (ProdFactor prods) = ProdFactor (map (happyPrepare' ts) prods)
happyPrepare' ts (ProdTerminal s) = ProdTerminal s
happyPrepare' ts (ProdOption prod) = ProdOption (happyPrepare' ts prod)
happyPrepare' ts (ProdRepeat prod) = ProdRepeat (happyPrepare' ts prod)
happyPrepare' ts (ProdRepeat1 prod) = ProdRepeat1 (happyPrepare' ts prod)
happyPrepare' ts (ProdRepeatWithAtom p1 p2) = ProdRepeatWithAtom (happyPrepare' ts p1) (happyPrepare' ts p2)
happyPrepare' ts (ProdPlus) = ProdPlus
happyPrepare' ts (ProdSlash prod) = ProdSlash (happyPrepare' ts prod)
happyPrepare' ts (ProdTerm prods) = ProdTerm (map (happyPrepare' ts) prods)
happyPrepare' ts (ProdNonterminal s) 
     | s `elem` ts = ProdTerminal s
     | otherwise   = ProdNonterminal s


yaccPrepare happyresult =
  [noDup (getNt nt) | nt <- nub nonterminals]
  where (nonterminals, prods) = transform happyresult [] []
        getNt str = [yaccPrepare' nonterminals p | p@(ProdProduction nt _ _) <- prods, str == nt] 
        transform [] as bs = (as,bs)
        transform ((ProdProduction nt aliases (ProdTerm ps)):pss) as bs =
              transform pss' (nt:as) bs'
              where (factors, pss') = span isProdFactor pss 
                    bs' = bs ++ [ProdProduction nt aliases (ProdTerm ps')]
                    ps' = ps ++ factors       
        noDup [p] = p
        noDup (ProdProduction nt aliases (ProdTerm ps):p':ps') = 
              ProdProduction nt aliases 
               (ProdTerm (foldr (\ (ProdProduction _ _ (ProdTerm prods')) ps1 -> ps1++prods') ps (p':ps')))
        isProdFactor p = case p of { ProdFactor _ -> True;  _ -> False}

yaccPrepare' nts (ProdProduction s1 s2 prod) = ProdProduction s1 s2 (yaccPrepare' nts prod)
yaccPrepare' nts (ProdFactor prods) = ProdFactor (map (yaccPrepare' nts) prods)
yaccPrepare' nts (ProdTerm prods) = ProdTerm (map (yaccPrepare' nts) prods)
yaccPrepare' nts (ProdOption prod) = ProdOption (yaccPrepare' nts prod)
yaccPrepare' nts (ProdRepeat prod) = ProdRepeat (yaccPrepare' nts prod)
yaccPrepare' nts (ProdRepeat1 prod) = ProdRepeat1 (yaccPrepare' nts prod)
yaccPrepare' nts (ProdRepeatWithAtom p1 p2) = ProdRepeatWithAtom (yaccPrepare' nts p1) (yaccPrepare' nts p2)
yaccPrepare' nts (ProdPlus) = ProdPlus
yaccPrepare' nts (ProdSlash prod) = ProdSlash (yaccPrepare' nts prod)
yaccPrepare' nts (ProdTerminal s) 
    | s `elem` nts = ProdNonterminal s
    | otherwise = ProdTerminal s


theEbnfParser  = ebnf2psParser . ebnf_postlexer  . lexer . uncomment
theHappyParser = ebnf2psParser . happy_postlexer . lexer . unlit
theYaccParser  = ebnf2psParser . yacc_postlexer  . lexer . yaccpreprocessor

-- $Id: Ebnf2psParser.hs,v 1.1.1.1 1998/12/09 13:34:08 pjt Exp $

{-
	The stack is in the following order throughout the parse:

	i	current token number
	j	another copy of this to avoid messing with the stack
	tk	current token semantic value
	st	current state
	sts	state stack
	stk	semantic stack
-}

-----------------------------------------------------------------------------

happyParse = happyNewToken action_0 [] []

-- All this HappyState stuff is simply because we can't have recursive
-- types in Haskell without an intervening data structure.

newtype HappyState b c = HappyState
        (Int ->                         -- token number
         Int ->                         -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)

-----------------------------------------------------------------------------
-- Accepting the parse

happyAccept j tk st sts [ HappyAbsSyn1 ans ] = happyReturn ans
happyAccept j tk st sts _                    = notHappyAtAll

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (-1) tk st sts stk@(HappyErrorToken i : _) =
--     _trace "shifting the error token" $
     new_state i i tk (HappyState new_state) (st:sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (st:sts) (HappyTerminal tk:stk)

-----------------------------------------------------------------------------
-- Reducing

-- happyReduce is specialised for the common cases.

-- don't allow reductions when we're in error recovery, because this can
-- lead to an infinite loop.

happySpecReduce_0 i fn (-1) tk _ sts stk
     = case sts of
	st@(HappyState action):sts -> action (-1) (-1) tk st sts stk
	_ -> happyError
happySpecReduce_0 i fn j tk st@(HappyState action) sts stk
     = action i j tk st (st:sts) (fn : stk)

happySpecReduce_1 i fn (-1) tk _ (st@(HappyState action):sts) stk
     = action (-1) (-1) tk st sts stk
happySpecReduce_1 i fn j tk _ sts@(st@(HappyState action):_) (v1:stk')
     = action i j tk st sts (fn v1 : stk')
happySpecReduce_1 _ _ _ _ _ _ _
     = notHappyAtAll

happySpecReduce_2 i fn (-1) tk _ (st@(HappyState action):sts) stk
     = action (-1) (-1) tk st sts stk
happySpecReduce_2 i fn j tk _ (_:sts@(st@(HappyState action):_)) (v1:v2:stk')
     = action i j tk st sts (fn v1 v2 : stk')
happySpecReduce_2 _ _ _ _ _ _ _
     = notHappyAtAll

happySpecReduce_3 i fn (-1) tk _ (st@(HappyState action):sts) stk
     = action (-1) (-1) tk st sts stk
happySpecReduce_3 i fn j tk _ (_:_:sts@(st@(HappyState action):_)) 
	(v1:v2:v3:stk')
     = action i j tk st sts (fn v1 v2 v3 : stk')
happySpecReduce_3 _ _ _ _ _ _ _
     = notHappyAtAll

happyReduce k i fn (-1) tk _ (st@(HappyState action):sts) stk
     = action (-1) (-1) tk st sts stk
happyReduce k i fn j tk st sts stk = action i j tk st' sts' (fn stk)
       where sts'@(st'@(HappyState action):_) = drop (k::Int) (st:sts)

happyMonadReduce k i c fn (-1) tk _ sts stk
      = case sts of
	     (st@(HappyState action):sts) -> action (-1) (-1) tk st sts stk
	     [] -> happyError
happyMonadReduce k i c fn j tk st sts stk =
	happyThen (fn stk) (\r -> action i j tk st' sts' (c r : stk'))
       where sts'@(st'@(HappyState action):_) = drop (k::Int) (st:sts)
	     stk' = drop (k::Int) stk

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto action j tk st = action j j tk (HappyState action)

-----------------------------------------------------------------------------
-- Error recovery (-1 is the error token)

-- fail if we are in recovery and no more states to discard
happyFail  (-1) tk st' [] stk = happyError

-- discard a state
happyFail  (-1) tk st' (st@(HappyState action):sts) stk =
--	_trace "discarding state" $
	action (-1) (-1) tk st sts stk

-- Enter error recovery: generate an error token,
-- 			 save the old token and carry on.

-- we push the error token on the stack in anticipation of a shift,
-- and also because this is a convenient place to store the saved token.

happyFail  i tk st@(HappyState action) sts stk =
--	_trace "entering error recovery" $
	action (-1) (-1) tk st sts (HappyErrorToken i : stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-- end of Happy Template.
