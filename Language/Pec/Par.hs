{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Language.Pec.Par where
import Language.Pec.Abs
import Language.Pec.Lex
import Language.Pec.ErrM

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (String)
	| HappyAbsSyn5 (Char)
	| HappyAbsSyn6 (Frac)
	| HappyAbsSyn7 (Uident)
	| HappyAbsSyn8 (Lident)
	| HappyAbsSyn9 (USym)
	| HappyAbsSyn10 (Number)
	| HappyAbsSyn11 (Count)
	| HappyAbsSyn12 (Module)
	| HappyAbsSyn13 (ExportDecl)
	| HappyAbsSyn14 (Export)
	| HappyAbsSyn15 (Spec)
	| HappyAbsSyn16 (TopDecl)
	| HappyAbsSyn17 (AsSpec)
	| HappyAbsSyn18 (ExtNm)
	| HappyAbsSyn19 (Exp)
	| HappyAbsSyn26 (UnOp)
	| HappyAbsSyn27 (CaseAlt)
	| HappyAbsSyn28 (CasePat)
	| HappyAbsSyn29 (BranchAlt)
	| HappyAbsSyn30 (BranchPat)
	| HappyAbsSyn31 (Type)
	| HappyAbsSyn35 (TyDecl)
	| HappyAbsSyn36 (ConC)
	| HappyAbsSyn37 (FieldT)
	| HappyAbsSyn38 (Lit)
	| HappyAbsSyn39 (FieldD)
	| HappyAbsSyn40 (Var)
	| HappyAbsSyn41 (Con)
	| HappyAbsSyn42 (Modid)
	| HappyAbsSyn43 (Field)
	| HappyAbsSyn44 (TyVar)
	| HappyAbsSyn45 ([Exp])
	| HappyAbsSyn46 ([FieldT])
	| HappyAbsSyn47 ([TyVar])
	| HappyAbsSyn48 ([Type])
	| HappyAbsSyn49 ([Export])
	| HappyAbsSyn51 ([ConC])
	| HappyAbsSyn53 ([FieldD])
	| HappyAbsSyn55 ([TopDecl])
	| HappyAbsSyn56 ([CaseAlt])
	| HappyAbsSyn57 ([BranchAlt])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

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
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201,
 action_202,
 action_203 :: () => Int -> ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

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
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103,
 happyReduce_104,
 happyReduce_105,
 happyReduce_106,
 happyReduce_107,
 happyReduce_108,
 happyReduce_109,
 happyReduce_110,
 happyReduce_111,
 happyReduce_112,
 happyReduce_113,
 happyReduce_114,
 happyReduce_115,
 happyReduce_116,
 happyReduce_117,
 happyReduce_118 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

action_0 (84) = happyShift action_4
action_0 (12) = happyGoto action_3
action_0 _ = happyFail

action_1 (91) = happyShift action_2
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (100) = happyAccept
action_3 _ = happyFail

action_4 (94) = happyShift action_7
action_4 (7) = happyGoto action_5
action_4 (42) = happyGoto action_6
action_4 _ = happyFail

action_5 _ = happyReduce_82

action_6 (79) = happyShift action_8
action_6 _ = happyFail

action_7 _ = happyReduce_4

action_8 (60) = happyShift action_10
action_8 (74) = happyShift action_11
action_8 (13) = happyGoto action_9
action_8 _ = happyFail

action_9 (87) = happyShift action_19
action_9 _ = happyFail

action_10 (94) = happyShift action_7
action_10 (95) = happyShift action_18
action_10 (7) = happyGoto action_12
action_10 (8) = happyGoto action_13
action_10 (14) = happyGoto action_14
action_10 (40) = happyGoto action_15
action_10 (41) = happyGoto action_16
action_10 (49) = happyGoto action_17
action_10 _ = happyReduce_97

action_11 _ = happyReduce_10

action_12 _ = happyReduce_81

action_13 _ = happyReduce_80

action_14 (62) = happyShift action_24
action_14 _ = happyReduce_98

action_15 _ = happyReduce_13

action_16 (60) = happyShift action_23
action_16 (15) = happyGoto action_22
action_16 _ = happyReduce_14

action_17 (61) = happyShift action_21
action_17 _ = happyFail

action_18 _ = happyReduce_5

action_19 (88) = happyShift action_20
action_19 _ = happyFail

action_20 (80) = happyShift action_31
action_20 (81) = happyShift action_32
action_20 (86) = happyShift action_33
action_20 (95) = happyShift action_18
action_20 (8) = happyGoto action_13
action_20 (16) = happyGoto action_28
action_20 (40) = happyGoto action_29
action_20 (55) = happyGoto action_30
action_20 _ = happyReduce_110

action_21 _ = happyReduce_11

action_22 _ = happyReduce_12

action_23 (64) = happyShift action_26
action_23 (65) = happyShift action_27
action_23 _ = happyFail

action_24 (94) = happyShift action_7
action_24 (95) = happyShift action_18
action_24 (7) = happyGoto action_12
action_24 (8) = happyGoto action_13
action_24 (14) = happyGoto action_14
action_24 (40) = happyGoto action_15
action_24 (41) = happyGoto action_16
action_24 (49) = happyGoto action_25
action_24 _ = happyReduce_97

action_25 _ = happyReduce_99

action_26 (61) = happyShift action_60
action_26 _ = happyFail

action_27 (61) = happyShift action_59
action_27 _ = happyFail

action_28 (67) = happyShift action_58
action_28 _ = happyReduce_111

action_29 (60) = happyShift action_49
action_29 (66) = happyShift action_50
action_29 (69) = happyShift action_51
action_29 (71) = happyShift action_52
action_29 (88) = happyShift action_53
action_29 (91) = happyShift action_2
action_29 (92) = happyShift action_54
action_29 (93) = happyShift action_55
action_29 (94) = happyShift action_7
action_29 (95) = happyShift action_18
action_29 (97) = happyShift action_56
action_29 (98) = happyShift action_57
action_29 (4) = happyGoto action_39
action_29 (5) = happyGoto action_40
action_29 (6) = happyGoto action_41
action_29 (7) = happyGoto action_12
action_29 (8) = happyGoto action_13
action_29 (10) = happyGoto action_42
action_29 (11) = happyGoto action_43
action_29 (25) = happyGoto action_44
action_29 (38) = happyGoto action_45
action_29 (40) = happyGoto action_46
action_29 (41) = happyGoto action_47
action_29 (52) = happyGoto action_48
action_29 _ = happyFail

action_30 (90) = happyShift action_38
action_30 _ = happyFail

action_31 (91) = happyShift action_2
action_31 (4) = happyGoto action_36
action_31 (18) = happyGoto action_37
action_31 _ = happyReduce_26

action_32 (94) = happyShift action_7
action_32 (7) = happyGoto action_5
action_32 (42) = happyGoto action_35
action_32 _ = happyFail

action_33 (94) = happyShift action_7
action_33 (7) = happyGoto action_12
action_33 (41) = happyGoto action_34
action_33 _ = happyFail

action_34 (47) = happyGoto action_98
action_34 _ = happyReduce_92

action_35 (75) = happyShift action_97
action_35 (17) = happyGoto action_96
action_35 _ = happyReduce_24

action_36 _ = happyReduce_25

action_37 (95) = happyShift action_18
action_37 (8) = happyGoto action_13
action_37 (40) = happyGoto action_95
action_37 _ = happyFail

action_38 _ = happyReduce_9

action_39 _ = happyReduce_75

action_40 _ = happyReduce_74

action_41 _ = happyReduce_77

action_42 _ = happyReduce_76

action_43 _ = happyReduce_48

action_44 (60) = happyShift action_49
action_44 (71) = happyShift action_52
action_44 (88) = happyShift action_53
action_44 (91) = happyShift action_2
action_44 (92) = happyShift action_54
action_44 (93) = happyShift action_55
action_44 (94) = happyShift action_7
action_44 (95) = happyShift action_18
action_44 (97) = happyShift action_56
action_44 (98) = happyShift action_57
action_44 (4) = happyGoto action_39
action_44 (5) = happyGoto action_40
action_44 (6) = happyGoto action_41
action_44 (7) = happyGoto action_12
action_44 (8) = happyGoto action_13
action_44 (10) = happyGoto action_42
action_44 (11) = happyGoto action_43
action_44 (25) = happyGoto action_44
action_44 (38) = happyGoto action_45
action_44 (40) = happyGoto action_46
action_44 (41) = happyGoto action_47
action_44 (52) = happyGoto action_94
action_44 _ = happyReduce_104

action_45 _ = happyReduce_50

action_46 _ = happyReduce_49

action_47 _ = happyReduce_78

action_48 (69) = happyShift action_93
action_48 _ = happyFail

action_49 (60) = happyShift action_49
action_49 (70) = happyShift action_75
action_49 (71) = happyShift action_52
action_49 (76) = happyShift action_76
action_49 (77) = happyShift action_77
action_49 (78) = happyShift action_78
action_49 (83) = happyShift action_79
action_49 (88) = happyShift action_53
action_49 (91) = happyShift action_2
action_49 (92) = happyShift action_54
action_49 (93) = happyShift action_55
action_49 (94) = happyShift action_7
action_49 (95) = happyShift action_18
action_49 (97) = happyShift action_56
action_49 (98) = happyShift action_57
action_49 (4) = happyGoto action_39
action_49 (5) = happyGoto action_40
action_49 (6) = happyGoto action_41
action_49 (7) = happyGoto action_12
action_49 (8) = happyGoto action_13
action_49 (10) = happyGoto action_42
action_49 (11) = happyGoto action_43
action_49 (19) = happyGoto action_91
action_49 (20) = happyGoto action_68
action_49 (21) = happyGoto action_69
action_49 (22) = happyGoto action_70
action_49 (23) = happyGoto action_71
action_49 (24) = happyGoto action_72
action_49 (25) = happyGoto action_73
action_49 (26) = happyGoto action_74
action_49 (38) = happyGoto action_45
action_49 (40) = happyGoto action_46
action_49 (41) = happyGoto action_47
action_49 (45) = happyGoto action_92
action_49 _ = happyReduce_86

action_50 (59) = happyShift action_88
action_50 (60) = happyShift action_89
action_50 (71) = happyShift action_90
action_50 (94) = happyShift action_7
action_50 (95) = happyShift action_18
action_50 (98) = happyShift action_57
action_50 (7) = happyGoto action_12
action_50 (8) = happyGoto action_80
action_50 (11) = happyGoto action_81
action_50 (31) = happyGoto action_82
action_50 (32) = happyGoto action_83
action_50 (33) = happyGoto action_84
action_50 (34) = happyGoto action_85
action_50 (41) = happyGoto action_86
action_50 (44) = happyGoto action_87
action_50 _ = happyFail

action_51 (60) = happyShift action_49
action_51 (70) = happyShift action_75
action_51 (71) = happyShift action_52
action_51 (76) = happyShift action_76
action_51 (77) = happyShift action_77
action_51 (78) = happyShift action_78
action_51 (83) = happyShift action_79
action_51 (88) = happyShift action_53
action_51 (91) = happyShift action_2
action_51 (92) = happyShift action_54
action_51 (93) = happyShift action_55
action_51 (94) = happyShift action_7
action_51 (95) = happyShift action_18
action_51 (97) = happyShift action_56
action_51 (98) = happyShift action_57
action_51 (4) = happyGoto action_39
action_51 (5) = happyGoto action_40
action_51 (6) = happyGoto action_41
action_51 (7) = happyGoto action_12
action_51 (8) = happyGoto action_13
action_51 (10) = happyGoto action_42
action_51 (11) = happyGoto action_43
action_51 (19) = happyGoto action_67
action_51 (20) = happyGoto action_68
action_51 (21) = happyGoto action_69
action_51 (22) = happyGoto action_70
action_51 (23) = happyGoto action_71
action_51 (24) = happyGoto action_72
action_51 (25) = happyGoto action_73
action_51 (26) = happyGoto action_74
action_51 (38) = happyGoto action_45
action_51 (40) = happyGoto action_46
action_51 (41) = happyGoto action_47
action_51 _ = happyFail

action_52 (72) = happyShift action_66
action_52 _ = happyFail

action_53 (95) = happyShift action_18
action_53 (8) = happyGoto action_62
action_53 (39) = happyGoto action_63
action_53 (43) = happyGoto action_64
action_53 (53) = happyGoto action_65
action_53 _ = happyFail

action_54 _ = happyReduce_2

action_55 _ = happyReduce_3

action_56 _ = happyReduce_7

action_57 _ = happyReduce_8

action_58 (80) = happyShift action_31
action_58 (81) = happyShift action_32
action_58 (86) = happyShift action_33
action_58 (95) = happyShift action_18
action_58 (8) = happyGoto action_13
action_58 (16) = happyGoto action_28
action_58 (40) = happyGoto action_29
action_58 (55) = happyGoto action_61
action_58 _ = happyReduce_110

action_59 _ = happyReduce_16

action_60 _ = happyReduce_15

action_61 _ = happyReduce_112

action_62 _ = happyReduce_83

action_63 (62) = happyShift action_131
action_63 _ = happyReduce_106

action_64 (69) = happyShift action_130
action_64 _ = happyFail

action_65 (90) = happyShift action_129
action_65 _ = happyFail

action_66 (60) = happyShift action_49
action_66 (70) = happyShift action_75
action_66 (71) = happyShift action_52
action_66 (76) = happyShift action_76
action_66 (77) = happyShift action_77
action_66 (78) = happyShift action_78
action_66 (83) = happyShift action_79
action_66 (88) = happyShift action_53
action_66 (91) = happyShift action_2
action_66 (92) = happyShift action_54
action_66 (93) = happyShift action_55
action_66 (94) = happyShift action_7
action_66 (95) = happyShift action_18
action_66 (97) = happyShift action_56
action_66 (98) = happyShift action_57
action_66 (4) = happyGoto action_39
action_66 (5) = happyGoto action_40
action_66 (6) = happyGoto action_41
action_66 (7) = happyGoto action_12
action_66 (8) = happyGoto action_13
action_66 (10) = happyGoto action_42
action_66 (11) = happyGoto action_43
action_66 (19) = happyGoto action_127
action_66 (20) = happyGoto action_68
action_66 (21) = happyGoto action_69
action_66 (22) = happyGoto action_70
action_66 (23) = happyGoto action_71
action_66 (24) = happyGoto action_72
action_66 (25) = happyGoto action_73
action_66 (26) = happyGoto action_74
action_66 (38) = happyGoto action_45
action_66 (40) = happyGoto action_46
action_66 (41) = happyGoto action_47
action_66 (45) = happyGoto action_128
action_66 _ = happyReduce_86

action_67 _ = happyReduce_21

action_68 _ = happyReduce_28

action_69 (68) = happyShift action_125
action_69 (69) = happyShift action_126
action_69 _ = happyReduce_34

action_70 (60) = happyShift action_49
action_70 (70) = happyShift action_75
action_70 (71) = happyShift action_52
action_70 (88) = happyShift action_53
action_70 (91) = happyShift action_2
action_70 (92) = happyShift action_54
action_70 (93) = happyShift action_55
action_70 (94) = happyShift action_7
action_70 (95) = happyShift action_18
action_70 (96) = happyShift action_124
action_70 (97) = happyShift action_56
action_70 (98) = happyShift action_57
action_70 (4) = happyGoto action_39
action_70 (5) = happyGoto action_40
action_70 (6) = happyGoto action_41
action_70 (7) = happyGoto action_12
action_70 (8) = happyGoto action_13
action_70 (9) = happyGoto action_122
action_70 (10) = happyGoto action_42
action_70 (11) = happyGoto action_43
action_70 (23) = happyGoto action_123
action_70 (24) = happyGoto action_72
action_70 (25) = happyGoto action_73
action_70 (26) = happyGoto action_74
action_70 (38) = happyGoto action_45
action_70 (40) = happyGoto action_46
action_70 (41) = happyGoto action_47
action_70 _ = happyReduce_36

action_71 _ = happyReduce_38

action_72 (64) = happyShift action_120
action_72 (72) = happyShift action_121
action_72 _ = happyReduce_40

action_73 _ = happyReduce_43

action_74 (60) = happyShift action_49
action_74 (71) = happyShift action_52
action_74 (88) = happyShift action_53
action_74 (91) = happyShift action_2
action_74 (92) = happyShift action_54
action_74 (93) = happyShift action_55
action_74 (94) = happyShift action_7
action_74 (95) = happyShift action_18
action_74 (97) = happyShift action_56
action_74 (98) = happyShift action_57
action_74 (4) = happyGoto action_39
action_74 (5) = happyGoto action_40
action_74 (6) = happyGoto action_41
action_74 (7) = happyGoto action_12
action_74 (8) = happyGoto action_13
action_74 (10) = happyGoto action_42
action_74 (11) = happyGoto action_43
action_74 (24) = happyGoto action_119
action_74 (25) = happyGoto action_73
action_74 (38) = happyGoto action_45
action_74 (40) = happyGoto action_46
action_74 (41) = happyGoto action_47
action_74 _ = happyFail

action_75 _ = happyReduce_51

action_76 (85) = happyShift action_118
action_76 _ = happyFail

action_77 (60) = happyShift action_49
action_77 (70) = happyShift action_75
action_77 (71) = happyShift action_52
action_77 (76) = happyShift action_76
action_77 (77) = happyShift action_77
action_77 (78) = happyShift action_78
action_77 (83) = happyShift action_79
action_77 (88) = happyShift action_53
action_77 (91) = happyShift action_2
action_77 (92) = happyShift action_54
action_77 (93) = happyShift action_55
action_77 (94) = happyShift action_7
action_77 (95) = happyShift action_18
action_77 (97) = happyShift action_56
action_77 (98) = happyShift action_57
action_77 (4) = happyGoto action_39
action_77 (5) = happyGoto action_40
action_77 (6) = happyGoto action_41
action_77 (7) = happyGoto action_12
action_77 (8) = happyGoto action_13
action_77 (10) = happyGoto action_42
action_77 (11) = happyGoto action_43
action_77 (19) = happyGoto action_117
action_77 (20) = happyGoto action_68
action_77 (21) = happyGoto action_69
action_77 (22) = happyGoto action_70
action_77 (23) = happyGoto action_71
action_77 (24) = happyGoto action_72
action_77 (25) = happyGoto action_73
action_77 (26) = happyGoto action_74
action_77 (38) = happyGoto action_45
action_77 (40) = happyGoto action_46
action_77 (41) = happyGoto action_47
action_77 _ = happyFail

action_78 (88) = happyShift action_116
action_78 _ = happyFail

action_79 (60) = happyShift action_49
action_79 (70) = happyShift action_75
action_79 (71) = happyShift action_52
action_79 (88) = happyShift action_53
action_79 (91) = happyShift action_2
action_79 (92) = happyShift action_54
action_79 (93) = happyShift action_55
action_79 (94) = happyShift action_7
action_79 (95) = happyShift action_18
action_79 (97) = happyShift action_56
action_79 (98) = happyShift action_57
action_79 (4) = happyGoto action_39
action_79 (5) = happyGoto action_40
action_79 (6) = happyGoto action_41
action_79 (7) = happyGoto action_12
action_79 (8) = happyGoto action_13
action_79 (10) = happyGoto action_42
action_79 (11) = happyGoto action_43
action_79 (21) = happyGoto action_115
action_79 (22) = happyGoto action_70
action_79 (23) = happyGoto action_71
action_79 (24) = happyGoto action_72
action_79 (25) = happyGoto action_73
action_79 (26) = happyGoto action_74
action_79 (38) = happyGoto action_45
action_79 (40) = happyGoto action_46
action_79 (41) = happyGoto action_47
action_79 _ = happyFail

action_80 _ = happyReduce_84

action_81 _ = happyReduce_66

action_82 _ = happyReduce_20

action_83 (63) = happyShift action_114
action_83 _ = happyReduce_60

action_84 _ = happyReduce_63

action_85 _ = happyReduce_64

action_86 (59) = happyShift action_88
action_86 (60) = happyShift action_89
action_86 (94) = happyShift action_7
action_86 (95) = happyShift action_18
action_86 (98) = happyShift action_57
action_86 (7) = happyGoto action_12
action_86 (8) = happyGoto action_80
action_86 (11) = happyGoto action_81
action_86 (33) = happyGoto action_112
action_86 (34) = happyGoto action_85
action_86 (41) = happyGoto action_108
action_86 (44) = happyGoto action_87
action_86 (54) = happyGoto action_113
action_86 _ = happyReduce_68

action_87 _ = happyReduce_67

action_88 (95) = happyShift action_18
action_88 (8) = happyGoto action_111
action_88 _ = happyFail

action_89 (59) = happyShift action_88
action_89 (60) = happyShift action_89
action_89 (71) = happyShift action_90
action_89 (94) = happyShift action_7
action_89 (95) = happyShift action_18
action_89 (98) = happyShift action_57
action_89 (7) = happyGoto action_12
action_89 (8) = happyGoto action_80
action_89 (11) = happyGoto action_81
action_89 (31) = happyGoto action_109
action_89 (32) = happyGoto action_83
action_89 (33) = happyGoto action_84
action_89 (34) = happyGoto action_85
action_89 (41) = happyGoto action_86
action_89 (44) = happyGoto action_87
action_89 (48) = happyGoto action_110
action_89 _ = happyReduce_94

action_90 (59) = happyShift action_88
action_90 (60) = happyShift action_89
action_90 (94) = happyShift action_7
action_90 (95) = happyShift action_18
action_90 (98) = happyShift action_57
action_90 (7) = happyGoto action_12
action_90 (8) = happyGoto action_80
action_90 (11) = happyGoto action_81
action_90 (33) = happyGoto action_107
action_90 (34) = happyGoto action_85
action_90 (41) = happyGoto action_108
action_90 (44) = happyGoto action_87
action_90 _ = happyFail

action_91 (62) = happyShift action_105
action_91 (66) = happyShift action_106
action_91 _ = happyReduce_87

action_92 (61) = happyShift action_104
action_92 _ = happyFail

action_93 (60) = happyShift action_49
action_93 (70) = happyShift action_75
action_93 (71) = happyShift action_52
action_93 (76) = happyShift action_76
action_93 (77) = happyShift action_77
action_93 (78) = happyShift action_78
action_93 (83) = happyShift action_79
action_93 (88) = happyShift action_53
action_93 (91) = happyShift action_2
action_93 (92) = happyShift action_54
action_93 (93) = happyShift action_55
action_93 (94) = happyShift action_7
action_93 (95) = happyShift action_18
action_93 (97) = happyShift action_56
action_93 (98) = happyShift action_57
action_93 (4) = happyGoto action_39
action_93 (5) = happyGoto action_40
action_93 (6) = happyGoto action_41
action_93 (7) = happyGoto action_12
action_93 (8) = happyGoto action_13
action_93 (10) = happyGoto action_42
action_93 (11) = happyGoto action_43
action_93 (19) = happyGoto action_103
action_93 (20) = happyGoto action_68
action_93 (21) = happyGoto action_69
action_93 (22) = happyGoto action_70
action_93 (23) = happyGoto action_71
action_93 (24) = happyGoto action_72
action_93 (25) = happyGoto action_73
action_93 (26) = happyGoto action_74
action_93 (38) = happyGoto action_45
action_93 (40) = happyGoto action_46
action_93 (41) = happyGoto action_47
action_93 _ = happyFail

action_94 _ = happyReduce_105

action_95 (66) = happyShift action_102
action_95 _ = happyFail

action_96 _ = happyReduce_17

action_97 (94) = happyShift action_7
action_97 (7) = happyGoto action_12
action_97 (41) = happyGoto action_101
action_97 _ = happyFail

action_98 (59) = happyShift action_88
action_98 (69) = happyShift action_100
action_98 (95) = happyShift action_18
action_98 (8) = happyGoto action_80
action_98 (44) = happyGoto action_99
action_98 _ = happyFail

action_99 _ = happyReduce_93

action_100 (59) = happyShift action_88
action_100 (60) = happyShift action_89
action_100 (71) = happyShift action_90
action_100 (88) = happyShift action_155
action_100 (89) = happyShift action_156
action_100 (94) = happyShift action_7
action_100 (95) = happyShift action_18
action_100 (98) = happyShift action_57
action_100 (7) = happyGoto action_12
action_100 (8) = happyGoto action_80
action_100 (11) = happyGoto action_81
action_100 (31) = happyGoto action_153
action_100 (32) = happyGoto action_83
action_100 (33) = happyGoto action_84
action_100 (34) = happyGoto action_85
action_100 (35) = happyGoto action_154
action_100 (41) = happyGoto action_86
action_100 (44) = happyGoto action_87
action_100 _ = happyFail

action_101 _ = happyReduce_23

action_102 (59) = happyShift action_88
action_102 (60) = happyShift action_89
action_102 (71) = happyShift action_90
action_102 (94) = happyShift action_7
action_102 (95) = happyShift action_18
action_102 (98) = happyShift action_57
action_102 (7) = happyGoto action_12
action_102 (8) = happyGoto action_80
action_102 (11) = happyGoto action_81
action_102 (31) = happyGoto action_152
action_102 (32) = happyGoto action_83
action_102 (33) = happyGoto action_84
action_102 (34) = happyGoto action_85
action_102 (41) = happyGoto action_86
action_102 (44) = happyGoto action_87
action_102 _ = happyFail

action_103 _ = happyReduce_22

action_104 _ = happyReduce_46

action_105 (60) = happyShift action_49
action_105 (70) = happyShift action_75
action_105 (71) = happyShift action_52
action_105 (76) = happyShift action_76
action_105 (77) = happyShift action_77
action_105 (78) = happyShift action_78
action_105 (83) = happyShift action_79
action_105 (88) = happyShift action_53
action_105 (91) = happyShift action_2
action_105 (92) = happyShift action_54
action_105 (93) = happyShift action_55
action_105 (94) = happyShift action_7
action_105 (95) = happyShift action_18
action_105 (97) = happyShift action_56
action_105 (98) = happyShift action_57
action_105 (4) = happyGoto action_39
action_105 (5) = happyGoto action_40
action_105 (6) = happyGoto action_41
action_105 (7) = happyGoto action_12
action_105 (8) = happyGoto action_13
action_105 (10) = happyGoto action_42
action_105 (11) = happyGoto action_43
action_105 (19) = happyGoto action_127
action_105 (20) = happyGoto action_68
action_105 (21) = happyGoto action_69
action_105 (22) = happyGoto action_70
action_105 (23) = happyGoto action_71
action_105 (24) = happyGoto action_72
action_105 (25) = happyGoto action_73
action_105 (26) = happyGoto action_74
action_105 (38) = happyGoto action_45
action_105 (40) = happyGoto action_46
action_105 (41) = happyGoto action_47
action_105 (45) = happyGoto action_151
action_105 _ = happyReduce_86

action_106 (59) = happyShift action_88
action_106 (60) = happyShift action_89
action_106 (71) = happyShift action_90
action_106 (94) = happyShift action_7
action_106 (95) = happyShift action_18
action_106 (98) = happyShift action_57
action_106 (7) = happyGoto action_12
action_106 (8) = happyGoto action_80
action_106 (11) = happyGoto action_81
action_106 (31) = happyGoto action_150
action_106 (32) = happyGoto action_83
action_106 (33) = happyGoto action_84
action_106 (34) = happyGoto action_85
action_106 (41) = happyGoto action_86
action_106 (44) = happyGoto action_87
action_106 _ = happyFail

action_107 (59) = happyShift action_88
action_107 (60) = happyShift action_89
action_107 (94) = happyShift action_7
action_107 (95) = happyShift action_18
action_107 (98) = happyShift action_57
action_107 (7) = happyGoto action_12
action_107 (8) = happyGoto action_80
action_107 (11) = happyGoto action_81
action_107 (33) = happyGoto action_149
action_107 (34) = happyGoto action_85
action_107 (41) = happyGoto action_108
action_107 (44) = happyGoto action_87
action_107 _ = happyFail

action_108 _ = happyReduce_68

action_109 (62) = happyShift action_148
action_109 _ = happyReduce_95

action_110 (61) = happyShift action_147
action_110 _ = happyFail

action_111 _ = happyReduce_85

action_112 (59) = happyShift action_88
action_112 (60) = happyShift action_89
action_112 (94) = happyShift action_7
action_112 (95) = happyShift action_18
action_112 (98) = happyShift action_57
action_112 (7) = happyGoto action_12
action_112 (8) = happyGoto action_80
action_112 (11) = happyGoto action_81
action_112 (33) = happyGoto action_112
action_112 (34) = happyGoto action_85
action_112 (41) = happyGoto action_108
action_112 (44) = happyGoto action_87
action_112 (54) = happyGoto action_146
action_112 _ = happyReduce_108

action_113 _ = happyReduce_62

action_114 (59) = happyShift action_88
action_114 (60) = happyShift action_89
action_114 (71) = happyShift action_90
action_114 (94) = happyShift action_7
action_114 (95) = happyShift action_18
action_114 (98) = happyShift action_57
action_114 (7) = happyGoto action_12
action_114 (8) = happyGoto action_80
action_114 (11) = happyGoto action_81
action_114 (31) = happyGoto action_145
action_114 (32) = happyGoto action_83
action_114 (33) = happyGoto action_84
action_114 (34) = happyGoto action_85
action_114 (41) = happyGoto action_86
action_114 (44) = happyGoto action_87
action_114 _ = happyFail

action_115 (69) = happyShift action_144
action_115 _ = happyFail

action_116 (60) = happyShift action_49
action_116 (70) = happyShift action_75
action_116 (71) = happyShift action_52
action_116 (76) = happyShift action_76
action_116 (77) = happyShift action_77
action_116 (83) = happyShift action_79
action_116 (88) = happyShift action_53
action_116 (91) = happyShift action_2
action_116 (92) = happyShift action_54
action_116 (93) = happyShift action_55
action_116 (94) = happyShift action_7
action_116 (95) = happyShift action_18
action_116 (97) = happyShift action_56
action_116 (98) = happyShift action_57
action_116 (4) = happyGoto action_39
action_116 (5) = happyGoto action_40
action_116 (6) = happyGoto action_41
action_116 (7) = happyGoto action_12
action_116 (8) = happyGoto action_13
action_116 (10) = happyGoto action_42
action_116 (11) = happyGoto action_43
action_116 (20) = happyGoto action_142
action_116 (21) = happyGoto action_69
action_116 (22) = happyGoto action_70
action_116 (23) = happyGoto action_71
action_116 (24) = happyGoto action_72
action_116 (25) = happyGoto action_73
action_116 (26) = happyGoto action_74
action_116 (38) = happyGoto action_45
action_116 (40) = happyGoto action_46
action_116 (41) = happyGoto action_47
action_116 (58) = happyGoto action_143
action_116 _ = happyFail

action_117 (85) = happyShift action_141
action_117 _ = happyFail

action_118 (88) = happyShift action_140
action_118 _ = happyFail

action_119 (64) = happyShift action_120
action_119 (72) = happyShift action_121
action_119 _ = happyReduce_39

action_120 (95) = happyShift action_18
action_120 (8) = happyGoto action_62
action_120 (43) = happyGoto action_139
action_120 _ = happyFail

action_121 (60) = happyShift action_49
action_121 (70) = happyShift action_75
action_121 (71) = happyShift action_52
action_121 (76) = happyShift action_76
action_121 (77) = happyShift action_77
action_121 (78) = happyShift action_78
action_121 (83) = happyShift action_79
action_121 (88) = happyShift action_53
action_121 (91) = happyShift action_2
action_121 (92) = happyShift action_54
action_121 (93) = happyShift action_55
action_121 (94) = happyShift action_7
action_121 (95) = happyShift action_18
action_121 (97) = happyShift action_56
action_121 (98) = happyShift action_57
action_121 (4) = happyGoto action_39
action_121 (5) = happyGoto action_40
action_121 (6) = happyGoto action_41
action_121 (7) = happyGoto action_12
action_121 (8) = happyGoto action_13
action_121 (10) = happyGoto action_42
action_121 (11) = happyGoto action_43
action_121 (19) = happyGoto action_138
action_121 (20) = happyGoto action_68
action_121 (21) = happyGoto action_69
action_121 (22) = happyGoto action_70
action_121 (23) = happyGoto action_71
action_121 (24) = happyGoto action_72
action_121 (25) = happyGoto action_73
action_121 (26) = happyGoto action_74
action_121 (38) = happyGoto action_45
action_121 (40) = happyGoto action_46
action_121 (41) = happyGoto action_47
action_121 _ = happyFail

action_122 (60) = happyShift action_49
action_122 (70) = happyShift action_75
action_122 (71) = happyShift action_52
action_122 (88) = happyShift action_53
action_122 (91) = happyShift action_2
action_122 (92) = happyShift action_54
action_122 (93) = happyShift action_55
action_122 (94) = happyShift action_7
action_122 (95) = happyShift action_18
action_122 (97) = happyShift action_56
action_122 (98) = happyShift action_57
action_122 (4) = happyGoto action_39
action_122 (5) = happyGoto action_40
action_122 (6) = happyGoto action_41
action_122 (7) = happyGoto action_12
action_122 (8) = happyGoto action_13
action_122 (10) = happyGoto action_42
action_122 (11) = happyGoto action_43
action_122 (22) = happyGoto action_137
action_122 (23) = happyGoto action_71
action_122 (24) = happyGoto action_72
action_122 (25) = happyGoto action_73
action_122 (26) = happyGoto action_74
action_122 (38) = happyGoto action_45
action_122 (40) = happyGoto action_46
action_122 (41) = happyGoto action_47
action_122 _ = happyFail

action_123 _ = happyReduce_37

action_124 _ = happyReduce_6

action_125 (60) = happyShift action_49
action_125 (70) = happyShift action_75
action_125 (71) = happyShift action_52
action_125 (76) = happyShift action_76
action_125 (77) = happyShift action_77
action_125 (78) = happyShift action_78
action_125 (83) = happyShift action_79
action_125 (88) = happyShift action_53
action_125 (91) = happyShift action_2
action_125 (92) = happyShift action_54
action_125 (93) = happyShift action_55
action_125 (94) = happyShift action_7
action_125 (95) = happyShift action_18
action_125 (97) = happyShift action_56
action_125 (98) = happyShift action_57
action_125 (4) = happyGoto action_39
action_125 (5) = happyGoto action_40
action_125 (6) = happyGoto action_41
action_125 (7) = happyGoto action_12
action_125 (8) = happyGoto action_13
action_125 (10) = happyGoto action_42
action_125 (11) = happyGoto action_43
action_125 (19) = happyGoto action_136
action_125 (20) = happyGoto action_68
action_125 (21) = happyGoto action_69
action_125 (22) = happyGoto action_70
action_125 (23) = happyGoto action_71
action_125 (24) = happyGoto action_72
action_125 (25) = happyGoto action_73
action_125 (26) = happyGoto action_74
action_125 (38) = happyGoto action_45
action_125 (40) = happyGoto action_46
action_125 (41) = happyGoto action_47
action_125 _ = happyFail

action_126 (60) = happyShift action_49
action_126 (70) = happyShift action_75
action_126 (71) = happyShift action_52
action_126 (76) = happyShift action_76
action_126 (77) = happyShift action_77
action_126 (78) = happyShift action_78
action_126 (83) = happyShift action_79
action_126 (88) = happyShift action_53
action_126 (91) = happyShift action_2
action_126 (92) = happyShift action_54
action_126 (93) = happyShift action_55
action_126 (94) = happyShift action_7
action_126 (95) = happyShift action_18
action_126 (97) = happyShift action_56
action_126 (98) = happyShift action_57
action_126 (4) = happyGoto action_39
action_126 (5) = happyGoto action_40
action_126 (6) = happyGoto action_41
action_126 (7) = happyGoto action_12
action_126 (8) = happyGoto action_13
action_126 (10) = happyGoto action_42
action_126 (11) = happyGoto action_43
action_126 (19) = happyGoto action_135
action_126 (20) = happyGoto action_68
action_126 (21) = happyGoto action_69
action_126 (22) = happyGoto action_70
action_126 (23) = happyGoto action_71
action_126 (24) = happyGoto action_72
action_126 (25) = happyGoto action_73
action_126 (26) = happyGoto action_74
action_126 (38) = happyGoto action_45
action_126 (40) = happyGoto action_46
action_126 (41) = happyGoto action_47
action_126 _ = happyFail

action_127 (62) = happyShift action_105
action_127 _ = happyReduce_87

action_128 (73) = happyShift action_134
action_128 _ = happyFail

action_129 _ = happyReduce_45

action_130 (60) = happyShift action_49
action_130 (70) = happyShift action_75
action_130 (71) = happyShift action_52
action_130 (76) = happyShift action_76
action_130 (77) = happyShift action_77
action_130 (78) = happyShift action_78
action_130 (83) = happyShift action_79
action_130 (88) = happyShift action_53
action_130 (91) = happyShift action_2
action_130 (92) = happyShift action_54
action_130 (93) = happyShift action_55
action_130 (94) = happyShift action_7
action_130 (95) = happyShift action_18
action_130 (97) = happyShift action_56
action_130 (98) = happyShift action_57
action_130 (4) = happyGoto action_39
action_130 (5) = happyGoto action_40
action_130 (6) = happyGoto action_41
action_130 (7) = happyGoto action_12
action_130 (8) = happyGoto action_13
action_130 (10) = happyGoto action_42
action_130 (11) = happyGoto action_43
action_130 (19) = happyGoto action_133
action_130 (20) = happyGoto action_68
action_130 (21) = happyGoto action_69
action_130 (22) = happyGoto action_70
action_130 (23) = happyGoto action_71
action_130 (24) = happyGoto action_72
action_130 (25) = happyGoto action_73
action_130 (26) = happyGoto action_74
action_130 (38) = happyGoto action_45
action_130 (40) = happyGoto action_46
action_130 (41) = happyGoto action_47
action_130 _ = happyFail

action_131 (95) = happyShift action_18
action_131 (8) = happyGoto action_62
action_131 (39) = happyGoto action_63
action_131 (43) = happyGoto action_64
action_131 (53) = happyGoto action_132
action_131 _ = happyFail

action_132 _ = happyReduce_107

action_133 _ = happyReduce_79

action_134 _ = happyReduce_44

action_135 _ = happyReduce_29

action_136 _ = happyReduce_31

action_137 (60) = happyShift action_49
action_137 (70) = happyShift action_75
action_137 (71) = happyShift action_52
action_137 (88) = happyShift action_53
action_137 (91) = happyShift action_2
action_137 (92) = happyShift action_54
action_137 (93) = happyShift action_55
action_137 (94) = happyShift action_7
action_137 (95) = happyShift action_18
action_137 (97) = happyShift action_56
action_137 (98) = happyShift action_57
action_137 (4) = happyGoto action_39
action_137 (5) = happyGoto action_40
action_137 (6) = happyGoto action_41
action_137 (7) = happyGoto action_12
action_137 (8) = happyGoto action_13
action_137 (10) = happyGoto action_42
action_137 (11) = happyGoto action_43
action_137 (23) = happyGoto action_123
action_137 (24) = happyGoto action_72
action_137 (25) = happyGoto action_73
action_137 (26) = happyGoto action_74
action_137 (38) = happyGoto action_45
action_137 (40) = happyGoto action_46
action_137 (41) = happyGoto action_47
action_137 _ = happyReduce_35

action_138 (73) = happyShift action_172
action_138 _ = happyFail

action_139 _ = happyReduce_42

action_140 (89) = happyShift action_171
action_140 (29) = happyGoto action_169
action_140 (57) = happyGoto action_170
action_140 _ = happyFail

action_141 (88) = happyShift action_168
action_141 _ = happyFail

action_142 (67) = happyShift action_167
action_142 _ = happyReduce_117

action_143 (90) = happyShift action_166
action_143 _ = happyFail

action_144 (60) = happyShift action_49
action_144 (70) = happyShift action_75
action_144 (71) = happyShift action_52
action_144 (76) = happyShift action_76
action_144 (77) = happyShift action_77
action_144 (78) = happyShift action_78
action_144 (83) = happyShift action_79
action_144 (88) = happyShift action_53
action_144 (91) = happyShift action_2
action_144 (92) = happyShift action_54
action_144 (93) = happyShift action_55
action_144 (94) = happyShift action_7
action_144 (95) = happyShift action_18
action_144 (97) = happyShift action_56
action_144 (98) = happyShift action_57
action_144 (4) = happyGoto action_39
action_144 (5) = happyGoto action_40
action_144 (6) = happyGoto action_41
action_144 (7) = happyGoto action_12
action_144 (8) = happyGoto action_13
action_144 (10) = happyGoto action_42
action_144 (11) = happyGoto action_43
action_144 (19) = happyGoto action_165
action_144 (20) = happyGoto action_68
action_144 (21) = happyGoto action_69
action_144 (22) = happyGoto action_70
action_144 (23) = happyGoto action_71
action_144 (24) = happyGoto action_72
action_144 (25) = happyGoto action_73
action_144 (26) = happyGoto action_74
action_144 (38) = happyGoto action_45
action_144 (40) = happyGoto action_46
action_144 (41) = happyGoto action_47
action_144 _ = happyFail

action_145 _ = happyReduce_59

action_146 _ = happyReduce_109

action_147 _ = happyReduce_65

action_148 (59) = happyShift action_88
action_148 (60) = happyShift action_89
action_148 (71) = happyShift action_90
action_148 (94) = happyShift action_7
action_148 (95) = happyShift action_18
action_148 (98) = happyShift action_57
action_148 (7) = happyGoto action_12
action_148 (8) = happyGoto action_80
action_148 (11) = happyGoto action_81
action_148 (31) = happyGoto action_109
action_148 (32) = happyGoto action_83
action_148 (33) = happyGoto action_84
action_148 (34) = happyGoto action_85
action_148 (41) = happyGoto action_86
action_148 (44) = happyGoto action_87
action_148 (48) = happyGoto action_164
action_148 _ = happyReduce_94

action_149 _ = happyReduce_61

action_150 (61) = happyShift action_163
action_150 _ = happyFail

action_151 _ = happyReduce_88

action_152 _ = happyReduce_18

action_153 _ = happyReduce_71

action_154 _ = happyReduce_19

action_155 (95) = happyShift action_18
action_155 (8) = happyGoto action_62
action_155 (37) = happyGoto action_160
action_155 (43) = happyGoto action_161
action_155 (46) = happyGoto action_162
action_155 _ = happyReduce_89

action_156 (94) = happyShift action_7
action_156 (7) = happyGoto action_12
action_156 (36) = happyGoto action_157
action_156 (41) = happyGoto action_158
action_156 (51) = happyGoto action_159
action_156 _ = happyFail

action_157 (89) = happyShift action_189
action_157 _ = happyReduce_102

action_158 (50) = happyGoto action_188
action_158 _ = happyReduce_100

action_159 _ = happyReduce_70

action_160 (62) = happyShift action_187
action_160 _ = happyReduce_90

action_161 (66) = happyShift action_186
action_161 _ = happyFail

action_162 (90) = happyShift action_185
action_162 _ = happyFail

action_163 _ = happyReduce_47

action_164 _ = happyReduce_96

action_165 (82) = happyShift action_184
action_165 _ = happyFail

action_166 _ = happyReduce_27

action_167 (60) = happyShift action_49
action_167 (70) = happyShift action_75
action_167 (71) = happyShift action_52
action_167 (76) = happyShift action_76
action_167 (77) = happyShift action_77
action_167 (83) = happyShift action_79
action_167 (88) = happyShift action_53
action_167 (91) = happyShift action_2
action_167 (92) = happyShift action_54
action_167 (93) = happyShift action_55
action_167 (94) = happyShift action_7
action_167 (95) = happyShift action_18
action_167 (97) = happyShift action_56
action_167 (98) = happyShift action_57
action_167 (4) = happyGoto action_39
action_167 (5) = happyGoto action_40
action_167 (6) = happyGoto action_41
action_167 (7) = happyGoto action_12
action_167 (8) = happyGoto action_13
action_167 (10) = happyGoto action_42
action_167 (11) = happyGoto action_43
action_167 (20) = happyGoto action_142
action_167 (21) = happyGoto action_69
action_167 (22) = happyGoto action_70
action_167 (23) = happyGoto action_71
action_167 (24) = happyGoto action_72
action_167 (25) = happyGoto action_73
action_167 (26) = happyGoto action_74
action_167 (38) = happyGoto action_45
action_167 (40) = happyGoto action_46
action_167 (41) = happyGoto action_47
action_167 (58) = happyGoto action_183
action_167 _ = happyFail

action_168 (91) = happyShift action_2
action_168 (92) = happyShift action_54
action_168 (93) = happyShift action_55
action_168 (94) = happyShift action_7
action_168 (95) = happyShift action_18
action_168 (97) = happyShift action_56
action_168 (4) = happyGoto action_39
action_168 (5) = happyGoto action_40
action_168 (6) = happyGoto action_41
action_168 (7) = happyGoto action_12
action_168 (8) = happyGoto action_13
action_168 (10) = happyGoto action_42
action_168 (27) = happyGoto action_177
action_168 (28) = happyGoto action_178
action_168 (38) = happyGoto action_179
action_168 (40) = happyGoto action_180
action_168 (41) = happyGoto action_181
action_168 (56) = happyGoto action_182
action_168 _ = happyFail

action_169 (67) = happyShift action_176
action_169 _ = happyReduce_115

action_170 (90) = happyShift action_175
action_170 _ = happyFail

action_171 (60) = happyShift action_49
action_171 (70) = happyShift action_75
action_171 (71) = happyShift action_52
action_171 (88) = happyShift action_53
action_171 (91) = happyShift action_2
action_171 (92) = happyShift action_54
action_171 (93) = happyShift action_55
action_171 (94) = happyShift action_7
action_171 (95) = happyShift action_18
action_171 (97) = happyShift action_56
action_171 (98) = happyShift action_57
action_171 (4) = happyGoto action_39
action_171 (5) = happyGoto action_40
action_171 (6) = happyGoto action_41
action_171 (7) = happyGoto action_12
action_171 (8) = happyGoto action_13
action_171 (10) = happyGoto action_42
action_171 (11) = happyGoto action_43
action_171 (21) = happyGoto action_173
action_171 (22) = happyGoto action_70
action_171 (23) = happyGoto action_71
action_171 (24) = happyGoto action_72
action_171 (25) = happyGoto action_73
action_171 (26) = happyGoto action_74
action_171 (30) = happyGoto action_174
action_171 (38) = happyGoto action_45
action_171 (40) = happyGoto action_46
action_171 (41) = happyGoto action_47
action_171 _ = happyReduce_58

action_172 _ = happyReduce_41

action_173 _ = happyReduce_57

action_174 (63) = happyShift action_200
action_174 _ = happyFail

action_175 _ = happyReduce_33

action_176 (89) = happyShift action_171
action_176 (29) = happyGoto action_169
action_176 (57) = happyGoto action_199
action_176 _ = happyFail

action_177 (67) = happyShift action_198
action_177 _ = happyReduce_113

action_178 (63) = happyShift action_197
action_178 _ = happyFail

action_179 _ = happyReduce_54

action_180 _ = happyReduce_55

action_181 (95) = happyShift action_18
action_181 (8) = happyGoto action_13
action_181 (40) = happyGoto action_196
action_181 _ = happyReduce_78

action_182 (90) = happyShift action_195
action_182 _ = happyFail

action_183 _ = happyReduce_118

action_184 (60) = happyShift action_49
action_184 (70) = happyShift action_75
action_184 (71) = happyShift action_52
action_184 (76) = happyShift action_76
action_184 (77) = happyShift action_77
action_184 (78) = happyShift action_78
action_184 (83) = happyShift action_79
action_184 (88) = happyShift action_53
action_184 (91) = happyShift action_2
action_184 (92) = happyShift action_54
action_184 (93) = happyShift action_55
action_184 (94) = happyShift action_7
action_184 (95) = happyShift action_18
action_184 (97) = happyShift action_56
action_184 (98) = happyShift action_57
action_184 (4) = happyGoto action_39
action_184 (5) = happyGoto action_40
action_184 (6) = happyGoto action_41
action_184 (7) = happyGoto action_12
action_184 (8) = happyGoto action_13
action_184 (10) = happyGoto action_42
action_184 (11) = happyGoto action_43
action_184 (19) = happyGoto action_194
action_184 (20) = happyGoto action_68
action_184 (21) = happyGoto action_69
action_184 (22) = happyGoto action_70
action_184 (23) = happyGoto action_71
action_184 (24) = happyGoto action_72
action_184 (25) = happyGoto action_73
action_184 (26) = happyGoto action_74
action_184 (38) = happyGoto action_45
action_184 (40) = happyGoto action_46
action_184 (41) = happyGoto action_47
action_184 _ = happyFail

action_185 _ = happyReduce_69

action_186 (59) = happyShift action_88
action_186 (60) = happyShift action_89
action_186 (71) = happyShift action_90
action_186 (94) = happyShift action_7
action_186 (95) = happyShift action_18
action_186 (98) = happyShift action_57
action_186 (7) = happyGoto action_12
action_186 (8) = happyGoto action_80
action_186 (11) = happyGoto action_81
action_186 (31) = happyGoto action_193
action_186 (32) = happyGoto action_83
action_186 (33) = happyGoto action_84
action_186 (34) = happyGoto action_85
action_186 (41) = happyGoto action_86
action_186 (44) = happyGoto action_87
action_186 _ = happyFail

action_187 (95) = happyShift action_18
action_187 (8) = happyGoto action_62
action_187 (37) = happyGoto action_160
action_187 (43) = happyGoto action_161
action_187 (46) = happyGoto action_192
action_187 _ = happyReduce_89

action_188 (59) = happyShift action_88
action_188 (60) = happyShift action_89
action_188 (94) = happyShift action_7
action_188 (95) = happyShift action_18
action_188 (98) = happyShift action_57
action_188 (7) = happyGoto action_12
action_188 (8) = happyGoto action_80
action_188 (11) = happyGoto action_81
action_188 (34) = happyGoto action_191
action_188 (41) = happyGoto action_108
action_188 (44) = happyGoto action_87
action_188 _ = happyReduce_72

action_189 (94) = happyShift action_7
action_189 (7) = happyGoto action_12
action_189 (36) = happyGoto action_157
action_189 (41) = happyGoto action_158
action_189 (51) = happyGoto action_190
action_189 _ = happyFail

action_190 _ = happyReduce_103

action_191 _ = happyReduce_101

action_192 _ = happyReduce_91

action_193 _ = happyReduce_73

action_194 _ = happyReduce_30

action_195 _ = happyReduce_32

action_196 _ = happyReduce_53

action_197 (60) = happyShift action_49
action_197 (70) = happyShift action_75
action_197 (71) = happyShift action_52
action_197 (76) = happyShift action_76
action_197 (77) = happyShift action_77
action_197 (78) = happyShift action_78
action_197 (83) = happyShift action_79
action_197 (88) = happyShift action_53
action_197 (91) = happyShift action_2
action_197 (92) = happyShift action_54
action_197 (93) = happyShift action_55
action_197 (94) = happyShift action_7
action_197 (95) = happyShift action_18
action_197 (97) = happyShift action_56
action_197 (98) = happyShift action_57
action_197 (4) = happyGoto action_39
action_197 (5) = happyGoto action_40
action_197 (6) = happyGoto action_41
action_197 (7) = happyGoto action_12
action_197 (8) = happyGoto action_13
action_197 (10) = happyGoto action_42
action_197 (11) = happyGoto action_43
action_197 (19) = happyGoto action_203
action_197 (20) = happyGoto action_68
action_197 (21) = happyGoto action_69
action_197 (22) = happyGoto action_70
action_197 (23) = happyGoto action_71
action_197 (24) = happyGoto action_72
action_197 (25) = happyGoto action_73
action_197 (26) = happyGoto action_74
action_197 (38) = happyGoto action_45
action_197 (40) = happyGoto action_46
action_197 (41) = happyGoto action_47
action_197 _ = happyFail

action_198 (91) = happyShift action_2
action_198 (92) = happyShift action_54
action_198 (93) = happyShift action_55
action_198 (94) = happyShift action_7
action_198 (95) = happyShift action_18
action_198 (97) = happyShift action_56
action_198 (4) = happyGoto action_39
action_198 (5) = happyGoto action_40
action_198 (6) = happyGoto action_41
action_198 (7) = happyGoto action_12
action_198 (8) = happyGoto action_13
action_198 (10) = happyGoto action_42
action_198 (27) = happyGoto action_177
action_198 (28) = happyGoto action_178
action_198 (38) = happyGoto action_179
action_198 (40) = happyGoto action_180
action_198 (41) = happyGoto action_181
action_198 (56) = happyGoto action_202
action_198 _ = happyFail

action_199 _ = happyReduce_116

action_200 (60) = happyShift action_49
action_200 (70) = happyShift action_75
action_200 (71) = happyShift action_52
action_200 (76) = happyShift action_76
action_200 (77) = happyShift action_77
action_200 (78) = happyShift action_78
action_200 (83) = happyShift action_79
action_200 (88) = happyShift action_53
action_200 (91) = happyShift action_2
action_200 (92) = happyShift action_54
action_200 (93) = happyShift action_55
action_200 (94) = happyShift action_7
action_200 (95) = happyShift action_18
action_200 (97) = happyShift action_56
action_200 (98) = happyShift action_57
action_200 (4) = happyGoto action_39
action_200 (5) = happyGoto action_40
action_200 (6) = happyGoto action_41
action_200 (7) = happyGoto action_12
action_200 (8) = happyGoto action_13
action_200 (10) = happyGoto action_42
action_200 (11) = happyGoto action_43
action_200 (19) = happyGoto action_201
action_200 (20) = happyGoto action_68
action_200 (21) = happyGoto action_69
action_200 (22) = happyGoto action_70
action_200 (23) = happyGoto action_71
action_200 (24) = happyGoto action_72
action_200 (25) = happyGoto action_73
action_200 (26) = happyGoto action_74
action_200 (38) = happyGoto action_45
action_200 (40) = happyGoto action_46
action_200 (41) = happyGoto action_47
action_200 _ = happyFail

action_201 _ = happyReduce_56

action_202 _ = happyReduce_114

action_203 _ = happyReduce_52

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (TC happy_var_1)))
	 =  HappyAbsSyn5
		 ((read ( happy_var_1)) :: Char
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (PT _ (T_Frac happy_var_1)))
	 =  HappyAbsSyn6
		 (Frac (happy_var_1)
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyTerminal (PT _ (T_Uident happy_var_1)))
	 =  HappyAbsSyn7
		 (Uident (happy_var_1)
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  8 happyReduction_5
happyReduction_5 (HappyTerminal (PT _ (T_Lident happy_var_1)))
	 =  HappyAbsSyn8
		 (Lident (happy_var_1)
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  9 happyReduction_6
happyReduction_6 (HappyTerminal (PT _ (T_USym happy_var_1)))
	 =  HappyAbsSyn9
		 (USym (happy_var_1)
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  10 happyReduction_7
happyReduction_7 (HappyTerminal (PT _ (T_Number happy_var_1)))
	 =  HappyAbsSyn10
		 (Number (happy_var_1)
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  11 happyReduction_8
happyReduction_8 (HappyTerminal (PT _ (T_Count happy_var_1)))
	 =  HappyAbsSyn11
		 (Count (happy_var_1)
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happyReduce 8 12 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn55  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn42  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Module happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_1  13 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn13
		 (ExpAllD
	)

happyReduce_11 = happySpecReduce_3  13 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn49  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (ExpListD happy_var_2
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  14 happyReduction_12
happyReduction_12 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn14
		 (TypeEx happy_var_1 happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  14 happyReduction_13
happyReduction_13 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn14
		 (VarEx happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_0  15 happyReduction_14
happyReduction_14  =  HappyAbsSyn15
		 (Neither
	)

happyReduce_15 = happySpecReduce_3  15 happyReduction_15
happyReduction_15 _
	_
	_
	 =  HappyAbsSyn15
		 (Decon
	)

happyReduce_16 = happySpecReduce_3  15 happyReduction_16
happyReduction_16 _
	_
	_
	 =  HappyAbsSyn15
		 (Both
	)

happyReduce_17 = happySpecReduce_3  16 happyReduction_17
happyReduction_17 (HappyAbsSyn17  happy_var_3)
	(HappyAbsSyn42  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (ImportD happy_var_2 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 5 16 happyReduction_18
happyReduction_18 ((HappyAbsSyn31  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn40  happy_var_3) `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (ExternD happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 5 16 happyReduction_19
happyReduction_19 ((HappyAbsSyn35  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn47  happy_var_3) `HappyStk`
	(HappyAbsSyn41  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (TypeD happy_var_2 (reverse happy_var_3) happy_var_5
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_3  16 happyReduction_20
happyReduction_20 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn16
		 (AscribeD happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  16 happyReduction_21
happyReduction_21 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn16
		 (VarD happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 4 16 happyReduction_22
happyReduction_22 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn45  happy_var_2) `HappyStk`
	(HappyAbsSyn40  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (ProcD happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_2  17 happyReduction_23
happyReduction_23 (HappyAbsSyn41  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (AsAS happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_0  17 happyReduction_24
happyReduction_24  =  HappyAbsSyn17
		 (EmptyAS
	)

happyReduce_25 = happySpecReduce_1  18 happyReduction_25
happyReduction_25 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn18
		 (SomeNm happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_0  18 happyReduction_26
happyReduction_26  =  HappyAbsSyn18
		 (NoneNm
	)

happyReduce_27 = happyReduce 4 19 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn45  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (BlockE happy_var_3
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_1  19 happyReduction_28
happyReduction_28 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  20 happyReduction_29
happyReduction_29 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (LetS happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happyReduce 6 20 happyReduction_30
happyReduction_30 ((HappyAbsSyn19  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (LetE happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_3  20 happyReduction_31
happyReduction_31 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (StoreE happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happyReduce 6 20 happyReduction_32
happyReduction_32 (_ `HappyStk`
	(HappyAbsSyn56  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (CaseE happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 5 20 happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyAbsSyn57  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (BranchE happy_var_4
	) `HappyStk` happyRest

happyReduce_34 = happySpecReduce_1  20 happyReduction_34
happyReduction_34 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  21 happyReduction_35
happyReduction_35 (HappyAbsSyn19  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (BinOpE happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  21 happyReduction_36
happyReduction_36 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  22 happyReduction_37
happyReduction_37 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (AppE happy_var_1 happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  22 happyReduction_38
happyReduction_38 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  23 happyReduction_39
happyReduction_39 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn19
		 (UnOpE happy_var_1 happy_var_2
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  23 happyReduction_40
happyReduction_40 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happyReduce 4 24 happyReduction_41
happyReduction_41 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (IdxE happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_42 = happySpecReduce_3  24 happyReduction_42
happyReduction_42 (HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (FldE happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  24 happyReduction_43
happyReduction_43 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happyReduce 4 25 happyReduction_44
happyReduction_44 (_ `HappyStk`
	(HappyAbsSyn45  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (ArrayE happy_var_3
	) `HappyStk` happyRest

happyReduce_45 = happySpecReduce_3  25 happyReduction_45
happyReduction_45 _
	(HappyAbsSyn53  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (RecordE happy_var_2
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  25 happyReduction_46
happyReduction_46 _
	(HappyAbsSyn45  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (TupleE happy_var_2
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happyReduce 5 25 happyReduction_47
happyReduction_47 (_ `HappyStk`
	(HappyAbsSyn31  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (AscribeE happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_48 = happySpecReduce_1  25 happyReduction_48
happyReduction_48 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn19
		 (CountE happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  25 happyReduction_49
happyReduction_49 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn19
		 (VarE happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  25 happyReduction_50
happyReduction_50 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn19
		 (LitE happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  26 happyReduction_51
happyReduction_51 _
	 =  HappyAbsSyn26
		 (Load
	)

happyReduce_52 = happySpecReduce_3  27 happyReduction_52
happyReduction_52 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 (CaseAlt happy_var_1 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  28 happyReduction_53
happyReduction_53 (HappyAbsSyn40  happy_var_2)
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn28
		 (ConP happy_var_1 happy_var_2
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  28 happyReduction_54
happyReduction_54 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn28
		 (LitP happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  28 happyReduction_55
happyReduction_55 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn28
		 (VarP happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happyReduce 4 29 happyReduction_56
happyReduction_56 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 (BranchAlt happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_57 = happySpecReduce_1  30 happyReduction_57
happyReduction_57 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn30
		 (BoolBP happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_0  30 happyReduction_58
happyReduction_58  =  HappyAbsSyn30
		 (DefaultBP
	)

happyReduce_59 = happySpecReduce_3  31 happyReduction_59
happyReduction_59 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (TyFun happy_var_1 happy_var_3
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  31 happyReduction_60
happyReduction_60 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  32 happyReduction_61
happyReduction_61 (HappyAbsSyn31  happy_var_3)
	(HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn31
		 (TyArray happy_var_2 happy_var_3
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_2  32 happyReduction_62
happyReduction_62 (HappyAbsSyn48  happy_var_2)
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn31
		 (TyConstr happy_var_1 happy_var_2
	)
happyReduction_62 _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  32 happyReduction_63
happyReduction_63 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  33 happyReduction_64
happyReduction_64 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  34 happyReduction_65
happyReduction_65 _
	(HappyAbsSyn48  happy_var_2)
	_
	 =  HappyAbsSyn31
		 (TyTuple happy_var_2
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  34 happyReduction_66
happyReduction_66 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn31
		 (TyCount happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  34 happyReduction_67
happyReduction_67 (HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn31
		 (TyVarT happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  34 happyReduction_68
happyReduction_68 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn31
		 (TyConstr0 happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  35 happyReduction_69
happyReduction_69 _
	(HappyAbsSyn46  happy_var_2)
	_
	 =  HappyAbsSyn35
		 (TyRecord happy_var_2
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_2  35 happyReduction_70
happyReduction_70 (HappyAbsSyn51  happy_var_2)
	_
	 =  HappyAbsSyn35
		 (TyTagged happy_var_2
	)
happyReduction_70 _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  35 happyReduction_71
happyReduction_71 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn35
		 (TySyn happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_2  36 happyReduction_72
happyReduction_72 (HappyAbsSyn48  happy_var_2)
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn36
		 (ConC happy_var_1 (reverse happy_var_2)
	)
happyReduction_72 _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  37 happyReduction_73
happyReduction_73 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn37
		 (FieldT happy_var_1 happy_var_3
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  38 happyReduction_74
happyReduction_74 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn38
		 (CharL happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  38 happyReduction_75
happyReduction_75 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn38
		 (StringL happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  38 happyReduction_76
happyReduction_76 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn38
		 (IntL happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  38 happyReduction_77
happyReduction_77 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn38
		 (FracL happy_var_1
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  38 happyReduction_78
happyReduction_78 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn38
		 (EnumL happy_var_1
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_3  39 happyReduction_79
happyReduction_79 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn39
		 (FieldD happy_var_1 happy_var_3
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  40 happyReduction_80
happyReduction_80 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn40
		 (Var happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  41 happyReduction_81
happyReduction_81 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn41
		 (Con happy_var_1
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  42 happyReduction_82
happyReduction_82 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn42
		 (Modid happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  43 happyReduction_83
happyReduction_83 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn43
		 (Field happy_var_1
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_1  44 happyReduction_84
happyReduction_84 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn44
		 (VarTV happy_var_1
	)
happyReduction_84 _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_2  44 happyReduction_85
happyReduction_85 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn44
		 (CntTV happy_var_2
	)
happyReduction_85 _ _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_0  45 happyReduction_86
happyReduction_86  =  HappyAbsSyn45
		 ([]
	)

happyReduce_87 = happySpecReduce_1  45 happyReduction_87
happyReduction_87 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn45
		 ((:[]) happy_var_1
	)
happyReduction_87 _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_3  45 happyReduction_88
happyReduction_88 (HappyAbsSyn45  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn45
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_88 _ _ _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_0  46 happyReduction_89
happyReduction_89  =  HappyAbsSyn46
		 ([]
	)

happyReduce_90 = happySpecReduce_1  46 happyReduction_90
happyReduction_90 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn46
		 ((:[]) happy_var_1
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_3  46 happyReduction_91
happyReduction_91 (HappyAbsSyn46  happy_var_3)
	_
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn46
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_91 _ _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_0  47 happyReduction_92
happyReduction_92  =  HappyAbsSyn47
		 ([]
	)

happyReduce_93 = happySpecReduce_2  47 happyReduction_93
happyReduction_93 (HappyAbsSyn44  happy_var_2)
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn47
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_93 _ _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_0  48 happyReduction_94
happyReduction_94  =  HappyAbsSyn48
		 ([]
	)

happyReduce_95 = happySpecReduce_1  48 happyReduction_95
happyReduction_95 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn48
		 ((:[]) happy_var_1
	)
happyReduction_95 _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_3  48 happyReduction_96
happyReduction_96 (HappyAbsSyn48  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn48
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_96 _ _ _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_0  49 happyReduction_97
happyReduction_97  =  HappyAbsSyn49
		 ([]
	)

happyReduce_98 = happySpecReduce_1  49 happyReduction_98
happyReduction_98 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn49
		 ((:[]) happy_var_1
	)
happyReduction_98 _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_3  49 happyReduction_99
happyReduction_99 (HappyAbsSyn49  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn49
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_99 _ _ _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_0  50 happyReduction_100
happyReduction_100  =  HappyAbsSyn48
		 ([]
	)

happyReduce_101 = happySpecReduce_2  50 happyReduction_101
happyReduction_101 (HappyAbsSyn31  happy_var_2)
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_101 _ _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_1  51 happyReduction_102
happyReduction_102 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn51
		 ((:[]) happy_var_1
	)
happyReduction_102 _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_3  51 happyReduction_103
happyReduction_103 (HappyAbsSyn51  happy_var_3)
	_
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn51
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_103 _ _ _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_1  52 happyReduction_104
happyReduction_104 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn45
		 ((:[]) happy_var_1
	)
happyReduction_104 _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_2  52 happyReduction_105
happyReduction_105 (HappyAbsSyn45  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn45
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_105 _ _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_1  53 happyReduction_106
happyReduction_106 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn53
		 ((:[]) happy_var_1
	)
happyReduction_106 _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_3  53 happyReduction_107
happyReduction_107 (HappyAbsSyn53  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn53
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_107 _ _ _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_1  54 happyReduction_108
happyReduction_108 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn48
		 ((:[]) happy_var_1
	)
happyReduction_108 _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_2  54 happyReduction_109
happyReduction_109 (HappyAbsSyn48  happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn48
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_109 _ _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_0  55 happyReduction_110
happyReduction_110  =  HappyAbsSyn55
		 ([]
	)

happyReduce_111 = happySpecReduce_1  55 happyReduction_111
happyReduction_111 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn55
		 ((:[]) happy_var_1
	)
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_3  55 happyReduction_112
happyReduction_112 (HappyAbsSyn55  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn55
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_112 _ _ _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_1  56 happyReduction_113
happyReduction_113 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn56
		 ((:[]) happy_var_1
	)
happyReduction_113 _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_3  56 happyReduction_114
happyReduction_114 (HappyAbsSyn56  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn56
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_114 _ _ _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_1  57 happyReduction_115
happyReduction_115 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn57
		 ((:[]) happy_var_1
	)
happyReduction_115 _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_3  57 happyReduction_116
happyReduction_116 (HappyAbsSyn57  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn57
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_116 _ _ _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_1  58 happyReduction_117
happyReduction_117 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn45
		 ((:[]) happy_var_1
	)
happyReduction_117 _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_3  58 happyReduction_118
happyReduction_118 (HappyAbsSyn45  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn45
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_118 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 100 100 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 59;
	PT _ (TS _ 2) -> cont 60;
	PT _ (TS _ 3) -> cont 61;
	PT _ (TS _ 4) -> cont 62;
	PT _ (TS _ 5) -> cont 63;
	PT _ (TS _ 6) -> cont 64;
	PT _ (TS _ 7) -> cont 65;
	PT _ (TS _ 8) -> cont 66;
	PT _ (TS _ 9) -> cont 67;
	PT _ (TS _ 10) -> cont 68;
	PT _ (TS _ 11) -> cont 69;
	PT _ (TS _ 12) -> cont 70;
	PT _ (TS _ 13) -> cont 71;
	PT _ (TS _ 14) -> cont 72;
	PT _ (TS _ 15) -> cont 73;
	PT _ (TS _ 16) -> cont 74;
	PT _ (TS _ 17) -> cont 75;
	PT _ (TS _ 18) -> cont 76;
	PT _ (TS _ 19) -> cont 77;
	PT _ (TS _ 20) -> cont 78;
	PT _ (TS _ 21) -> cont 79;
	PT _ (TS _ 22) -> cont 80;
	PT _ (TS _ 23) -> cont 81;
	PT _ (TS _ 24) -> cont 82;
	PT _ (TS _ 25) -> cont 83;
	PT _ (TS _ 26) -> cont 84;
	PT _ (TS _ 27) -> cont 85;
	PT _ (TS _ 28) -> cont 86;
	PT _ (TS _ 29) -> cont 87;
	PT _ (TS _ 30) -> cont 88;
	PT _ (TS _ 31) -> cont 89;
	PT _ (TS _ 32) -> cont 90;
	PT _ (TL happy_dollar_dollar) -> cont 91;
	PT _ (TC happy_dollar_dollar) -> cont 92;
	PT _ (T_Frac happy_dollar_dollar) -> cont 93;
	PT _ (T_Uident happy_dollar_dollar) -> cont 94;
	PT _ (T_Lident happy_dollar_dollar) -> cont 95;
	PT _ (T_USym happy_dollar_dollar) -> cont 96;
	PT _ (T_Number happy_dollar_dollar) -> cont 97;
	PT _ (T_Count happy_dollar_dollar) -> cont 98;
	_ -> cont 99;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pModule tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn12 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 311 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
