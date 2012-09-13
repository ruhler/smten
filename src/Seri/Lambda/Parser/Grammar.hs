{-# OPTIONS_GHC -w #-}
module Seri.Lambda.Parser.Grammar (parse) where

import Data.Maybe

import Seri.Failable
import Seri.Lambda.IR
import Seri.Lambda.Modularity
import Seri.Lambda.Prelude
import Seri.Lambda.Sugar
import Seri.Lambda.Types

import Seri.Lambda.Parser.Monad
import Seri.Lambda.Parser.Lexer

-- parser produced by Happy Version 1.18.9

data HappyAbsSyn t77 t78 t79 t80 t81 t82 t83
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Module)
	| HappyAbsSyn5 (([Import], [Dec]))
	| HappyAbsSyn6 ([Import])
	| HappyAbsSyn7 (Import)
	| HappyAbsSyn8 ([PDec])
	| HappyAbsSyn10 ([String])
	| HappyAbsSyn11 (PDec)
	| HappyAbsSyn12 ([TopSig])
	| HappyAbsSyn13 (TopSig)
	| HappyAbsSyn14 ([(Pat, Exp)])
	| HappyAbsSyn15 ((Pat, Exp))
	| HappyAbsSyn16 ([(Name, Clause)])
	| HappyAbsSyn17 ((Name, Clause))
	| HappyAbsSyn19 (Type)
	| HappyAbsSyn22 (NType)
	| HappyAbsSyn24 (String)
	| HappyAbsSyn25 ([Class])
	| HappyAbsSyn26 (Class)
	| HappyAbsSyn27 ([ConRec])
	| HappyAbsSyn28 (ConRec)
	| HappyAbsSyn29 ([(Name, Type)])
	| HappyAbsSyn30 ((Name, Type))
	| HappyAbsSyn31 ((Name, [Pat]))
	| HappyAbsSyn32 (Exp)
	| HappyAbsSyn38 ([Match])
	| HappyAbsSyn39 (Match)
	| HappyAbsSyn40 ([Stmt])
	| HappyAbsSyn41 (Stmt)
	| HappyAbsSyn42 ([(Name, Exp)])
	| HappyAbsSyn43 ((Name, Exp))
	| HappyAbsSyn44 (Pat)
	| HappyAbsSyn46 ([Pat])
	| HappyAbsSyn48 (Sig)
	| HappyAbsSyn71 ([Type])
	| HappyAbsSyn72 ([Exp])
	| HappyAbsSyn74 (TyVar)
	| HappyAbsSyn75 ([TyVar])
	| HappyAbsSyn77 t77
	| HappyAbsSyn78 t78
	| HappyAbsSyn79 t79
	| HappyAbsSyn80 t80
	| HappyAbsSyn81 t81
	| HappyAbsSyn82 t82
	| HappyAbsSyn83 t83

action_0 (121) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (121) = happyShift action_2
action_1 _ = happyFail

action_2 (102) = happyShift action_5
action_2 (67) = happyGoto action_4
action_2 _ = happyFail

action_3 (124) = happyAccept
action_3 _ = happyFail

action_4 (95) = happyShift action_6
action_4 (112) = happyShift action_7
action_4 _ = happyFail

action_5 _ = happyReduce_134

action_6 (102) = happyShift action_10
action_6 _ = happyFail

action_7 (88) = happyShift action_9
action_7 (5) = happyGoto action_8
action_7 _ = happyFail

action_8 _ = happyReduce_1

action_9 (86) = happyShift action_19
action_9 (103) = happyShift action_20
action_9 (109) = happyShift action_21
action_9 (110) = happyShift action_22
action_9 (111) = happyShift action_23
action_9 (122) = happyShift action_24
action_9 (6) = happyGoto action_11
action_9 (7) = happyGoto action_12
action_9 (8) = happyGoto action_13
action_9 (9) = happyGoto action_14
action_9 (11) = happyGoto action_15
action_9 (18) = happyGoto action_16
action_9 (31) = happyGoto action_17
action_9 (51) = happyGoto action_18
action_9 _ = happyFail

action_10 _ = happyReduce_135

action_11 (94) = happyShift action_67
action_11 (77) = happyGoto action_66
action_11 _ = happyReduce_154

action_12 _ = happyReduce_5

action_13 (94) = happyShift action_65
action_13 (77) = happyGoto action_64
action_13 _ = happyReduce_154

action_14 _ = happyReduce_8

action_15 _ = happyReduce_14

action_16 _ = happyReduce_16

action_17 (97) = happyShift action_63
action_17 (32) = happyGoto action_62
action_17 _ = happyFail

action_18 (84) = happyShift action_56
action_18 (86) = happyShift action_57
action_18 (101) = happyShift action_58
action_18 (102) = happyShift action_59
action_18 (103) = happyShift action_20
action_18 (106) = happyShift action_60
action_18 (107) = happyShift action_61
action_18 (46) = happyGoto action_47
action_18 (47) = happyGoto action_48
action_18 (48) = happyGoto action_49
action_18 (49) = happyGoto action_50
action_18 (50) = happyGoto action_51
action_18 (51) = happyGoto action_52
action_18 (55) = happyGoto action_53
action_18 (66) = happyGoto action_54
action_18 (78) = happyGoto action_55
action_18 _ = happyReduce_156

action_19 (95) = happyShift action_45
action_19 (104) = happyShift action_46
action_19 (58) = happyGoto action_44
action_19 _ = happyFail

action_20 _ = happyReduce_111

action_21 (102) = happyShift action_43
action_21 (62) = happyGoto action_42
action_21 _ = happyFail

action_22 (102) = happyShift action_41
action_22 (64) = happyGoto action_40
action_22 _ = happyFail

action_23 (84) = happyShift action_35
action_23 (86) = happyShift action_36
action_23 (99) = happyShift action_37
action_23 (102) = happyShift action_38
action_23 (103) = happyShift action_39
action_23 (20) = happyGoto action_26
action_23 (21) = happyGoto action_27
action_23 (24) = happyGoto action_28
action_23 (25) = happyGoto action_29
action_23 (26) = happyGoto action_30
action_23 (62) = happyGoto action_31
action_23 (63) = happyGoto action_32
action_23 (64) = happyGoto action_33
action_23 (65) = happyGoto action_34
action_23 _ = happyFail

action_24 (102) = happyShift action_5
action_24 (67) = happyGoto action_25
action_24 _ = happyFail

action_25 (95) = happyShift action_6
action_25 _ = happyReduce_7

action_26 (84) = happyShift action_35
action_26 (86) = happyShift action_36
action_26 (92) = happyShift action_137
action_26 (99) = happyShift action_37
action_26 (102) = happyShift action_43
action_26 (103) = happyShift action_39
action_26 (21) = happyGoto action_136
action_26 (24) = happyGoto action_28
action_26 (62) = happyGoto action_31
action_26 (63) = happyGoto action_32
action_26 _ = happyFail

action_27 _ = happyReduce_31

action_28 _ = happyReduce_33

action_29 (102) = happyShift action_41
action_29 (26) = happyGoto action_135
action_29 (64) = happyGoto action_33
action_29 (65) = happyGoto action_34
action_29 _ = happyFail

action_30 (112) = happyShift action_134
action_30 _ = happyFail

action_31 _ = happyReduce_43

action_32 _ = happyReduce_34

action_33 _ = happyReduce_132

action_34 (84) = happyShift action_35
action_34 (86) = happyShift action_36
action_34 (99) = happyShift action_37
action_34 (102) = happyShift action_43
action_34 (103) = happyShift action_39
action_34 (21) = happyGoto action_132
action_34 (24) = happyGoto action_28
action_34 (62) = happyGoto action_31
action_34 (63) = happyGoto action_32
action_34 (76) = happyGoto action_133
action_34 _ = happyFail

action_35 (84) = happyShift action_35
action_35 (85) = happyShift action_131
action_35 (86) = happyShift action_36
action_35 (99) = happyShift action_37
action_35 (102) = happyShift action_43
action_35 (103) = happyShift action_39
action_35 (19) = happyGoto action_130
action_35 (20) = happyGoto action_125
action_35 (21) = happyGoto action_27
action_35 (24) = happyGoto action_28
action_35 (62) = happyGoto action_31
action_35 (63) = happyGoto action_32
action_35 _ = happyFail

action_36 (84) = happyShift action_35
action_36 (86) = happyShift action_36
action_36 (87) = happyShift action_128
action_36 (90) = happyShift action_129
action_36 (93) = happyShift action_106
action_36 (99) = happyShift action_37
action_36 (102) = happyShift action_43
action_36 (103) = happyShift action_39
action_36 (19) = happyGoto action_124
action_36 (20) = happyGoto action_125
action_36 (21) = happyGoto action_27
action_36 (24) = happyGoto action_28
action_36 (62) = happyGoto action_31
action_36 (63) = happyGoto action_32
action_36 (69) = happyGoto action_126
action_36 (71) = happyGoto action_127
action_36 _ = happyFail

action_37 (86) = happyShift action_122
action_37 (103) = happyShift action_39
action_37 (106) = happyShift action_123
action_37 (23) = happyGoto action_120
action_37 (63) = happyGoto action_121
action_37 _ = happyFail

action_38 (84) = happyReduce_131
action_38 (86) = happyReduce_131
action_38 (99) = happyReduce_131
action_38 (102) = happyReduce_131
action_38 (103) = happyReduce_131
action_38 _ = happyReduce_129

action_39 _ = happyReduce_130

action_40 (99) = happyShift action_118
action_40 (103) = happyShift action_39
action_40 (63) = happyGoto action_114
action_40 (74) = happyGoto action_115
action_40 (75) = happyGoto action_119
action_40 _ = happyFail

action_41 _ = happyReduce_131

action_42 (99) = happyShift action_118
action_42 (103) = happyShift action_39
action_42 (63) = happyGoto action_114
action_42 (74) = happyGoto action_115
action_42 (75) = happyGoto action_116
action_42 (83) = happyGoto action_117
action_42 _ = happyReduce_166

action_43 _ = happyReduce_129

action_44 (87) = happyShift action_113
action_44 _ = happyFail

action_45 _ = happyReduce_124

action_46 _ = happyReduce_123

action_47 (84) = happyShift action_56
action_47 (86) = happyShift action_57
action_47 (102) = happyShift action_59
action_47 (103) = happyShift action_20
action_47 (106) = happyShift action_60
action_47 (107) = happyShift action_61
action_47 (47) = happyGoto action_112
action_47 (48) = happyGoto action_49
action_47 (49) = happyGoto action_50
action_47 (50) = happyGoto action_51
action_47 (51) = happyGoto action_52
action_47 (55) = happyGoto action_53
action_47 (66) = happyGoto action_54
action_47 _ = happyReduce_155

action_48 _ = happyReduce_94

action_49 _ = happyReduce_97

action_50 _ = happyReduce_104

action_51 _ = happyReduce_96

action_52 _ = happyReduce_110

action_53 _ = happyReduce_108

action_54 _ = happyReduce_118

action_55 _ = happyReduce_57

action_56 (84) = happyShift action_56
action_56 (85) = happyShift action_111
action_56 (86) = happyShift action_57
action_56 (102) = happyShift action_59
action_56 (103) = happyShift action_20
action_56 (106) = happyShift action_60
action_56 (107) = happyShift action_61
action_56 (44) = happyGoto action_109
action_56 (45) = happyGoto action_97
action_56 (47) = happyGoto action_98
action_56 (48) = happyGoto action_99
action_56 (49) = happyGoto action_50
action_56 (50) = happyGoto action_51
action_56 (51) = happyGoto action_52
action_56 (55) = happyGoto action_53
action_56 (66) = happyGoto action_54
action_56 (73) = happyGoto action_110
action_56 _ = happyFail

action_57 (84) = happyShift action_56
action_57 (86) = happyShift action_57
action_57 (87) = happyShift action_105
action_57 (93) = happyShift action_106
action_57 (95) = happyShift action_45
action_57 (98) = happyShift action_107
action_57 (102) = happyShift action_59
action_57 (103) = happyShift action_20
action_57 (104) = happyShift action_46
action_57 (105) = happyShift action_108
action_57 (106) = happyShift action_60
action_57 (107) = happyShift action_61
action_57 (44) = happyGoto action_96
action_57 (45) = happyGoto action_97
action_57 (47) = happyGoto action_98
action_57 (48) = happyGoto action_99
action_57 (49) = happyGoto action_100
action_57 (50) = happyGoto action_51
action_57 (51) = happyGoto action_101
action_57 (55) = happyGoto action_53
action_57 (58) = happyGoto action_44
action_57 (60) = happyGoto action_102
action_57 (61) = happyGoto action_103
action_57 (66) = happyGoto action_54
action_57 (69) = happyGoto action_104
action_57 _ = happyFail

action_58 (84) = happyShift action_35
action_58 (86) = happyShift action_36
action_58 (99) = happyShift action_37
action_58 (102) = happyShift action_43
action_58 (103) = happyShift action_39
action_58 (19) = happyGoto action_93
action_58 (20) = happyGoto action_94
action_58 (21) = happyGoto action_27
action_58 (24) = happyGoto action_28
action_58 (25) = happyGoto action_95
action_58 (62) = happyGoto action_31
action_58 (63) = happyGoto action_32
action_58 _ = happyFail

action_59 _ = happyReduce_133

action_60 _ = happyReduce_98

action_61 _ = happyReduce_99

action_62 _ = happyReduce_17

action_63 (84) = happyShift action_82
action_63 (86) = happyShift action_83
action_63 (100) = happyShift action_84
action_63 (102) = happyShift action_59
action_63 (103) = happyShift action_85
action_63 (106) = happyShift action_86
action_63 (107) = happyShift action_87
action_63 (108) = happyShift action_88
action_63 (113) = happyShift action_89
action_63 (115) = happyShift action_90
action_63 (117) = happyShift action_91
action_63 (120) = happyShift action_92
action_63 (33) = happyGoto action_73
action_63 (34) = happyGoto action_74
action_63 (35) = happyGoto action_75
action_63 (36) = happyGoto action_76
action_63 (37) = happyGoto action_77
action_63 (48) = happyGoto action_78
action_63 (49) = happyGoto action_50
action_63 (52) = happyGoto action_79
action_63 (53) = happyGoto action_80
action_63 (55) = happyGoto action_53
action_63 (66) = happyGoto action_54
action_63 (68) = happyGoto action_81
action_63 _ = happyFail

action_64 (89) = happyShift action_72
action_64 _ = happyFail

action_65 (86) = happyShift action_19
action_65 (103) = happyShift action_20
action_65 (109) = happyShift action_21
action_65 (110) = happyShift action_22
action_65 (111) = happyShift action_23
action_65 (9) = happyGoto action_71
action_65 (11) = happyGoto action_15
action_65 (18) = happyGoto action_16
action_65 (31) = happyGoto action_17
action_65 (51) = happyGoto action_18
action_65 _ = happyReduce_153

action_66 (89) = happyShift action_70
action_66 _ = happyFail

action_67 (86) = happyShift action_19
action_67 (103) = happyShift action_20
action_67 (109) = happyShift action_21
action_67 (110) = happyShift action_22
action_67 (111) = happyShift action_23
action_67 (122) = happyShift action_24
action_67 (7) = happyGoto action_68
action_67 (8) = happyGoto action_69
action_67 (9) = happyGoto action_14
action_67 (11) = happyGoto action_15
action_67 (18) = happyGoto action_16
action_67 (31) = happyGoto action_17
action_67 (51) = happyGoto action_18
action_67 _ = happyReduce_153

action_68 _ = happyReduce_6

action_69 (94) = happyShift action_65
action_69 (77) = happyGoto action_183
action_69 _ = happyReduce_154

action_70 _ = happyReduce_3

action_71 _ = happyReduce_9

action_72 _ = happyReduce_4

action_73 _ = happyReduce_58

action_74 (95) = happyShift action_45
action_74 (98) = happyShift action_107
action_74 (104) = happyShift action_46
action_74 (105) = happyShift action_108
action_74 (56) = happyGoto action_179
action_74 (57) = happyGoto action_180
action_74 (58) = happyGoto action_174
action_74 (59) = happyGoto action_181
action_74 (60) = happyGoto action_182
action_74 (61) = happyGoto action_103
action_74 _ = happyReduce_59

action_75 (84) = happyShift action_82
action_75 (86) = happyShift action_83
action_75 (102) = happyShift action_59
action_75 (103) = happyShift action_85
action_75 (106) = happyShift action_86
action_75 (107) = happyShift action_87
action_75 (108) = happyShift action_88
action_75 (36) = happyGoto action_178
action_75 (37) = happyGoto action_77
action_75 (48) = happyGoto action_78
action_75 (49) = happyGoto action_50
action_75 (52) = happyGoto action_79
action_75 (53) = happyGoto action_80
action_75 (55) = happyGoto action_53
action_75 (66) = happyGoto action_54
action_75 (68) = happyGoto action_81
action_75 _ = happyReduce_66

action_76 (88) = happyShift action_177
action_76 _ = happyReduce_67

action_77 _ = happyReduce_71

action_78 _ = happyReduce_70

action_79 _ = happyReduce_69

action_80 _ = happyReduce_114

action_81 _ = happyReduce_115

action_82 (84) = happyShift action_82
action_82 (85) = happyShift action_111
action_82 (86) = happyShift action_83
action_82 (100) = happyShift action_84
action_82 (102) = happyShift action_59
action_82 (103) = happyShift action_85
action_82 (106) = happyShift action_86
action_82 (107) = happyShift action_87
action_82 (108) = happyShift action_88
action_82 (113) = happyShift action_89
action_82 (115) = happyShift action_90
action_82 (117) = happyShift action_91
action_82 (120) = happyShift action_92
action_82 (33) = happyGoto action_175
action_82 (34) = happyGoto action_74
action_82 (35) = happyGoto action_75
action_82 (36) = happyGoto action_76
action_82 (37) = happyGoto action_77
action_82 (48) = happyGoto action_78
action_82 (49) = happyGoto action_50
action_82 (52) = happyGoto action_79
action_82 (53) = happyGoto action_80
action_82 (55) = happyGoto action_53
action_82 (66) = happyGoto action_54
action_82 (68) = happyGoto action_81
action_82 (72) = happyGoto action_176
action_82 _ = happyFail

action_83 (84) = happyShift action_82
action_83 (86) = happyShift action_83
action_83 (87) = happyShift action_105
action_83 (93) = happyShift action_106
action_83 (95) = happyShift action_45
action_83 (98) = happyShift action_107
action_83 (100) = happyShift action_84
action_83 (102) = happyShift action_59
action_83 (103) = happyShift action_85
action_83 (104) = happyShift action_46
action_83 (105) = happyShift action_108
action_83 (106) = happyShift action_86
action_83 (107) = happyShift action_87
action_83 (108) = happyShift action_88
action_83 (113) = happyShift action_89
action_83 (115) = happyShift action_90
action_83 (117) = happyShift action_91
action_83 (120) = happyShift action_92
action_83 (33) = happyGoto action_171
action_83 (34) = happyGoto action_74
action_83 (35) = happyGoto action_75
action_83 (36) = happyGoto action_76
action_83 (37) = happyGoto action_77
action_83 (48) = happyGoto action_78
action_83 (49) = happyGoto action_100
action_83 (52) = happyGoto action_79
action_83 (53) = happyGoto action_172
action_83 (55) = happyGoto action_53
action_83 (57) = happyGoto action_173
action_83 (58) = happyGoto action_174
action_83 (60) = happyGoto action_102
action_83 (61) = happyGoto action_103
action_83 (66) = happyGoto action_54
action_83 (68) = happyGoto action_81
action_83 (69) = happyGoto action_104
action_83 _ = happyFail

action_84 (86) = happyShift action_170
action_84 (103) = happyShift action_20
action_84 (50) = happyGoto action_169
action_84 (51) = happyGoto action_52
action_84 _ = happyFail

action_85 _ = happyReduce_136

action_86 _ = happyReduce_76

action_87 _ = happyReduce_77

action_88 _ = happyReduce_78

action_89 (88) = happyShift action_168
action_89 _ = happyFail

action_90 (84) = happyShift action_82
action_90 (86) = happyShift action_83
action_90 (100) = happyShift action_84
action_90 (102) = happyShift action_59
action_90 (103) = happyShift action_85
action_90 (106) = happyShift action_86
action_90 (107) = happyShift action_87
action_90 (108) = happyShift action_88
action_90 (113) = happyShift action_89
action_90 (115) = happyShift action_90
action_90 (117) = happyShift action_91
action_90 (120) = happyShift action_92
action_90 (33) = happyGoto action_167
action_90 (34) = happyGoto action_74
action_90 (35) = happyGoto action_75
action_90 (36) = happyGoto action_76
action_90 (37) = happyGoto action_77
action_90 (48) = happyGoto action_78
action_90 (49) = happyGoto action_50
action_90 (52) = happyGoto action_79
action_90 (53) = happyGoto action_80
action_90 (55) = happyGoto action_53
action_90 (66) = happyGoto action_54
action_90 (68) = happyGoto action_81
action_90 _ = happyFail

action_91 (84) = happyShift action_82
action_91 (86) = happyShift action_83
action_91 (100) = happyShift action_84
action_91 (102) = happyShift action_59
action_91 (103) = happyShift action_85
action_91 (106) = happyShift action_86
action_91 (107) = happyShift action_87
action_91 (108) = happyShift action_88
action_91 (113) = happyShift action_89
action_91 (115) = happyShift action_90
action_91 (117) = happyShift action_91
action_91 (120) = happyShift action_92
action_91 (33) = happyGoto action_166
action_91 (34) = happyGoto action_74
action_91 (35) = happyGoto action_75
action_91 (36) = happyGoto action_76
action_91 (37) = happyGoto action_77
action_91 (48) = happyGoto action_78
action_91 (49) = happyGoto action_50
action_91 (52) = happyGoto action_79
action_91 (53) = happyGoto action_80
action_91 (55) = happyGoto action_53
action_91 (66) = happyGoto action_54
action_91 (68) = happyGoto action_81
action_91 _ = happyFail

action_92 (88) = happyShift action_165
action_92 _ = happyFail

action_93 _ = happyReduce_27

action_94 (84) = happyShift action_35
action_94 (86) = happyShift action_36
action_94 (90) = happyShift action_147
action_94 (92) = happyShift action_137
action_94 (99) = happyShift action_37
action_94 (102) = happyShift action_43
action_94 (103) = happyShift action_39
action_94 (21) = happyGoto action_136
action_94 (24) = happyGoto action_28
action_94 (62) = happyGoto action_31
action_94 (63) = happyGoto action_32
action_94 _ = happyReduce_29

action_95 (84) = happyShift action_35
action_95 (86) = happyShift action_36
action_95 (99) = happyShift action_37
action_95 (102) = happyShift action_43
action_95 (103) = happyShift action_39
action_95 (19) = happyGoto action_164
action_95 (20) = happyGoto action_125
action_95 (21) = happyGoto action_27
action_95 (24) = happyGoto action_28
action_95 (62) = happyGoto action_31
action_95 (63) = happyGoto action_32
action_95 _ = happyFail

action_96 (87) = happyShift action_162
action_96 (93) = happyShift action_163
action_96 _ = happyFail

action_97 (98) = happyShift action_161
action_97 _ = happyReduce_90

action_98 _ = happyReduce_93

action_99 (84) = happyShift action_56
action_99 (86) = happyShift action_57
action_99 (102) = happyShift action_59
action_99 (103) = happyShift action_20
action_99 (106) = happyShift action_60
action_99 (107) = happyShift action_61
action_99 (46) = happyGoto action_160
action_99 (47) = happyGoto action_48
action_99 (48) = happyGoto action_49
action_99 (49) = happyGoto action_50
action_99 (50) = happyGoto action_51
action_99 (51) = happyGoto action_52
action_99 (55) = happyGoto action_53
action_99 (66) = happyGoto action_54
action_99 _ = happyReduce_97

action_100 (101) = happyShift action_159
action_100 _ = happyReduce_104

action_101 (101) = happyShift action_158
action_101 _ = happyReduce_110

action_102 (87) = happyShift action_157
action_102 _ = happyFail

action_103 _ = happyReduce_127

action_104 (87) = happyShift action_156
action_104 (93) = happyShift action_146
action_104 _ = happyFail

action_105 _ = happyReduce_105

action_106 _ = happyReduce_137

action_107 _ = happyReduce_126

action_108 _ = happyReduce_128

action_109 _ = happyReduce_145

action_110 (85) = happyShift action_154
action_110 (93) = happyShift action_155
action_110 _ = happyFail

action_111 _ = happyReduce_106

action_112 _ = happyReduce_95

action_113 _ = happyReduce_112

action_114 _ = happyReduce_147

action_115 _ = happyReduce_149

action_116 (99) = happyShift action_118
action_116 (103) = happyShift action_39
action_116 (63) = happyGoto action_114
action_116 (74) = happyGoto action_150
action_116 _ = happyReduce_165

action_117 (97) = happyShift action_153
action_117 _ = happyFail

action_118 (103) = happyShift action_39
action_118 (63) = happyGoto action_152
action_118 _ = happyFail

action_119 (99) = happyShift action_118
action_119 (103) = happyShift action_39
action_119 (112) = happyShift action_151
action_119 (63) = happyGoto action_114
action_119 (74) = happyGoto action_150
action_119 _ = happyFail

action_120 _ = happyReduce_37

action_121 _ = happyReduce_41

action_122 (86) = happyShift action_122
action_122 (103) = happyShift action_39
action_122 (106) = happyShift action_123
action_122 (22) = happyGoto action_148
action_122 (23) = happyGoto action_149
action_122 (63) = happyGoto action_121
action_122 _ = happyFail

action_123 _ = happyReduce_40

action_124 _ = happyReduce_141

action_125 (84) = happyShift action_35
action_125 (86) = happyShift action_36
action_125 (90) = happyShift action_147
action_125 (99) = happyShift action_37
action_125 (102) = happyShift action_43
action_125 (103) = happyShift action_39
action_125 (21) = happyGoto action_136
action_125 (24) = happyGoto action_28
action_125 (62) = happyGoto action_31
action_125 (63) = happyGoto action_32
action_125 _ = happyReduce_29

action_126 (87) = happyShift action_145
action_126 (93) = happyShift action_146
action_126 _ = happyFail

action_127 (87) = happyShift action_143
action_127 (93) = happyShift action_144
action_127 _ = happyFail

action_128 _ = happyReduce_44

action_129 (87) = happyShift action_142
action_129 _ = happyFail

action_130 (85) = happyShift action_141
action_130 _ = happyFail

action_131 _ = happyReduce_45

action_132 _ = happyReduce_151

action_133 (84) = happyShift action_35
action_133 (86) = happyShift action_36
action_133 (99) = happyShift action_37
action_133 (102) = happyShift action_43
action_133 (103) = happyShift action_39
action_133 (21) = happyGoto action_140
action_133 (24) = happyGoto action_28
action_133 (62) = happyGoto action_31
action_133 (63) = happyGoto action_32
action_133 _ = happyReduce_49

action_134 (88) = happyShift action_139
action_134 _ = happyFail

action_135 (112) = happyShift action_138
action_135 _ = happyFail

action_136 _ = happyReduce_32

action_137 _ = happyReduce_48

action_138 (88) = happyShift action_230
action_138 _ = happyFail

action_139 (86) = happyShift action_19
action_139 (103) = happyShift action_20
action_139 (16) = happyGoto action_226
action_139 (17) = happyGoto action_227
action_139 (31) = happyGoto action_228
action_139 (51) = happyGoto action_229
action_139 _ = happyFail

action_140 _ = happyReduce_152

action_141 _ = happyReduce_36

action_142 _ = happyReduce_46

action_143 _ = happyReduce_35

action_144 (84) = happyShift action_35
action_144 (86) = happyShift action_36
action_144 (99) = happyShift action_37
action_144 (102) = happyShift action_43
action_144 (103) = happyShift action_39
action_144 (19) = happyGoto action_225
action_144 (20) = happyGoto action_125
action_144 (21) = happyGoto action_27
action_144 (24) = happyGoto action_28
action_144 (62) = happyGoto action_31
action_144 (63) = happyGoto action_32
action_144 _ = happyFail

action_145 _ = happyReduce_47

action_146 _ = happyReduce_138

action_147 (84) = happyShift action_35
action_147 (86) = happyShift action_36
action_147 (99) = happyShift action_37
action_147 (102) = happyShift action_43
action_147 (103) = happyShift action_39
action_147 (19) = happyGoto action_224
action_147 (20) = happyGoto action_125
action_147 (21) = happyGoto action_27
action_147 (24) = happyGoto action_28
action_147 (62) = happyGoto action_31
action_147 (63) = happyGoto action_32
action_147 _ = happyFail

action_148 (87) = happyShift action_223
action_148 _ = happyFail

action_149 (95) = happyShift action_45
action_149 (104) = happyShift action_46
action_149 (58) = happyGoto action_222
action_149 _ = happyReduce_38

action_150 _ = happyReduce_150

action_151 (88) = happyShift action_221
action_151 _ = happyFail

action_152 _ = happyReduce_148

action_153 (102) = happyShift action_220
action_153 (27) = happyGoto action_216
action_153 (28) = happyGoto action_217
action_153 (54) = happyGoto action_218
action_153 (80) = happyGoto action_219
action_153 _ = happyReduce_160

action_154 _ = happyReduce_102

action_155 (84) = happyShift action_56
action_155 (86) = happyShift action_57
action_155 (102) = happyShift action_59
action_155 (103) = happyShift action_20
action_155 (106) = happyShift action_60
action_155 (107) = happyShift action_61
action_155 (44) = happyGoto action_215
action_155 (45) = happyGoto action_97
action_155 (47) = happyGoto action_98
action_155 (48) = happyGoto action_99
action_155 (49) = happyGoto action_50
action_155 (50) = happyGoto action_51
action_155 (51) = happyGoto action_52
action_155 (55) = happyGoto action_53
action_155 (66) = happyGoto action_54
action_155 _ = happyFail

action_156 _ = happyReduce_107

action_157 _ = happyReduce_119

action_158 (84) = happyShift action_35
action_158 (86) = happyShift action_36
action_158 (99) = happyShift action_37
action_158 (102) = happyShift action_43
action_158 (103) = happyShift action_39
action_158 (19) = happyGoto action_214
action_158 (20) = happyGoto action_125
action_158 (21) = happyGoto action_27
action_158 (24) = happyGoto action_28
action_158 (62) = happyGoto action_31
action_158 (63) = happyGoto action_32
action_158 _ = happyFail

action_159 (84) = happyShift action_35
action_159 (86) = happyShift action_36
action_159 (99) = happyShift action_37
action_159 (102) = happyShift action_43
action_159 (103) = happyShift action_39
action_159 (19) = happyGoto action_213
action_159 (20) = happyGoto action_125
action_159 (21) = happyGoto action_27
action_159 (24) = happyGoto action_28
action_159 (62) = happyGoto action_31
action_159 (63) = happyGoto action_32
action_159 _ = happyFail

action_160 (84) = happyShift action_56
action_160 (86) = happyShift action_57
action_160 (102) = happyShift action_59
action_160 (103) = happyShift action_20
action_160 (106) = happyShift action_60
action_160 (107) = happyShift action_61
action_160 (47) = happyGoto action_112
action_160 (48) = happyGoto action_49
action_160 (49) = happyGoto action_50
action_160 (50) = happyGoto action_51
action_160 (51) = happyGoto action_52
action_160 (55) = happyGoto action_53
action_160 (66) = happyGoto action_54
action_160 _ = happyReduce_92

action_161 (84) = happyShift action_56
action_161 (86) = happyShift action_57
action_161 (102) = happyShift action_59
action_161 (103) = happyShift action_20
action_161 (106) = happyShift action_60
action_161 (107) = happyShift action_61
action_161 (44) = happyGoto action_212
action_161 (45) = happyGoto action_97
action_161 (47) = happyGoto action_98
action_161 (48) = happyGoto action_99
action_161 (49) = happyGoto action_50
action_161 (50) = happyGoto action_51
action_161 (51) = happyGoto action_52
action_161 (55) = happyGoto action_53
action_161 (66) = happyGoto action_54
action_161 _ = happyFail

action_162 _ = happyReduce_100

action_163 (84) = happyShift action_56
action_163 (86) = happyShift action_57
action_163 (102) = happyShift action_59
action_163 (103) = happyShift action_20
action_163 (106) = happyShift action_60
action_163 (107) = happyShift action_61
action_163 (44) = happyGoto action_109
action_163 (45) = happyGoto action_97
action_163 (47) = happyGoto action_98
action_163 (48) = happyGoto action_99
action_163 (49) = happyGoto action_50
action_163 (50) = happyGoto action_51
action_163 (51) = happyGoto action_52
action_163 (55) = happyGoto action_53
action_163 (66) = happyGoto action_54
action_163 (73) = happyGoto action_211
action_163 _ = happyFail

action_164 _ = happyReduce_28

action_165 (84) = happyShift action_82
action_165 (86) = happyShift action_208
action_165 (100) = happyShift action_84
action_165 (102) = happyShift action_59
action_165 (103) = happyShift action_209
action_165 (106) = happyShift action_86
action_165 (107) = happyShift action_87
action_165 (108) = happyShift action_88
action_165 (113) = happyShift action_210
action_165 (115) = happyShift action_90
action_165 (117) = happyShift action_91
action_165 (120) = happyShift action_92
action_165 (33) = happyGoto action_204
action_165 (34) = happyGoto action_74
action_165 (35) = happyGoto action_75
action_165 (36) = happyGoto action_76
action_165 (37) = happyGoto action_77
action_165 (40) = happyGoto action_205
action_165 (41) = happyGoto action_206
action_165 (48) = happyGoto action_78
action_165 (49) = happyGoto action_50
action_165 (50) = happyGoto action_207
action_165 (51) = happyGoto action_52
action_165 (52) = happyGoto action_79
action_165 (53) = happyGoto action_80
action_165 (55) = happyGoto action_53
action_165 (66) = happyGoto action_54
action_165 (68) = happyGoto action_81
action_165 _ = happyFail

action_166 (118) = happyShift action_203
action_166 _ = happyFail

action_167 (116) = happyShift action_202
action_167 _ = happyFail

action_168 (84) = happyShift action_56
action_168 (86) = happyShift action_57
action_168 (102) = happyShift action_59
action_168 (103) = happyShift action_20
action_168 (106) = happyShift action_60
action_168 (107) = happyShift action_61
action_168 (14) = happyGoto action_199
action_168 (15) = happyGoto action_200
action_168 (44) = happyGoto action_201
action_168 (45) = happyGoto action_97
action_168 (47) = happyGoto action_98
action_168 (48) = happyGoto action_99
action_168 (49) = happyGoto action_50
action_168 (50) = happyGoto action_51
action_168 (51) = happyGoto action_52
action_168 (55) = happyGoto action_53
action_168 (66) = happyGoto action_54
action_168 _ = happyFail

action_169 (90) = happyShift action_198
action_169 _ = happyFail

action_170 (86) = happyShift action_19
action_170 (95) = happyShift action_45
action_170 (103) = happyShift action_20
action_170 (104) = happyShift action_46
action_170 (51) = happyGoto action_197
action_170 (58) = happyGoto action_44
action_170 _ = happyFail

action_171 (87) = happyShift action_195
action_171 (93) = happyShift action_196
action_171 _ = happyFail

action_172 (101) = happyShift action_194
action_172 _ = happyReduce_114

action_173 (87) = happyShift action_193
action_173 _ = happyFail

action_174 _ = happyReduce_122

action_175 _ = happyReduce_143

action_176 (85) = happyShift action_191
action_176 (93) = happyShift action_192
action_176 _ = happyFail

action_177 (86) = happyShift action_190
action_177 (103) = happyShift action_85
action_177 (42) = happyGoto action_186
action_177 (43) = happyGoto action_187
action_177 (53) = happyGoto action_188
action_177 (68) = happyGoto action_81
action_177 (82) = happyGoto action_189
action_177 _ = happyReduce_164

action_178 (88) = happyShift action_177
action_178 _ = happyReduce_68

action_179 (84) = happyShift action_82
action_179 (86) = happyShift action_83
action_179 (100) = happyShift action_84
action_179 (102) = happyShift action_59
action_179 (103) = happyShift action_85
action_179 (106) = happyShift action_86
action_179 (107) = happyShift action_87
action_179 (108) = happyShift action_88
action_179 (113) = happyShift action_89
action_179 (115) = happyShift action_90
action_179 (117) = happyShift action_91
action_179 (120) = happyShift action_92
action_179 (34) = happyGoto action_185
action_179 (35) = happyGoto action_75
action_179 (36) = happyGoto action_76
action_179 (37) = happyGoto action_77
action_179 (48) = happyGoto action_78
action_179 (49) = happyGoto action_50
action_179 (52) = happyGoto action_79
action_179 (53) = happyGoto action_80
action_179 (55) = happyGoto action_53
action_179 (66) = happyGoto action_54
action_179 (68) = happyGoto action_81
action_179 _ = happyFail

action_180 _ = happyReduce_120

action_181 _ = happyReduce_121

action_182 _ = happyReduce_125

action_183 (89) = happyShift action_184
action_183 _ = happyFail

action_184 _ = happyReduce_2

action_185 _ = happyReduce_60

action_186 (93) = happyShift action_268
action_186 _ = happyReduce_163

action_187 _ = happyReduce_87

action_188 (97) = happyShift action_267
action_188 _ = happyFail

action_189 (89) = happyShift action_266
action_189 _ = happyFail

action_190 (95) = happyShift action_45
action_190 (104) = happyShift action_46
action_190 (57) = happyGoto action_173
action_190 (58) = happyGoto action_174
action_190 _ = happyFail

action_191 _ = happyReduce_74

action_192 (84) = happyShift action_82
action_192 (86) = happyShift action_83
action_192 (100) = happyShift action_84
action_192 (102) = happyShift action_59
action_192 (103) = happyShift action_85
action_192 (106) = happyShift action_86
action_192 (107) = happyShift action_87
action_192 (108) = happyShift action_88
action_192 (113) = happyShift action_89
action_192 (115) = happyShift action_90
action_192 (117) = happyShift action_91
action_192 (120) = happyShift action_92
action_192 (33) = happyGoto action_265
action_192 (34) = happyGoto action_74
action_192 (35) = happyGoto action_75
action_192 (36) = happyGoto action_76
action_192 (37) = happyGoto action_77
action_192 (48) = happyGoto action_78
action_192 (49) = happyGoto action_50
action_192 (52) = happyGoto action_79
action_192 (53) = happyGoto action_80
action_192 (55) = happyGoto action_53
action_192 (66) = happyGoto action_54
action_192 (68) = happyGoto action_81
action_192 _ = happyFail

action_193 _ = happyReduce_116

action_194 (84) = happyShift action_35
action_194 (86) = happyShift action_36
action_194 (99) = happyShift action_37
action_194 (102) = happyShift action_43
action_194 (103) = happyShift action_39
action_194 (19) = happyGoto action_264
action_194 (20) = happyGoto action_125
action_194 (21) = happyGoto action_27
action_194 (24) = happyGoto action_28
action_194 (62) = happyGoto action_31
action_194 (63) = happyGoto action_32
action_194 _ = happyFail

action_195 _ = happyReduce_72

action_196 (84) = happyShift action_82
action_196 (86) = happyShift action_83
action_196 (100) = happyShift action_84
action_196 (102) = happyShift action_59
action_196 (103) = happyShift action_85
action_196 (106) = happyShift action_86
action_196 (107) = happyShift action_87
action_196 (108) = happyShift action_88
action_196 (113) = happyShift action_89
action_196 (115) = happyShift action_90
action_196 (117) = happyShift action_91
action_196 (120) = happyShift action_92
action_196 (33) = happyGoto action_175
action_196 (34) = happyGoto action_74
action_196 (35) = happyGoto action_75
action_196 (36) = happyGoto action_76
action_196 (37) = happyGoto action_77
action_196 (48) = happyGoto action_78
action_196 (49) = happyGoto action_50
action_196 (52) = happyGoto action_79
action_196 (53) = happyGoto action_80
action_196 (55) = happyGoto action_53
action_196 (66) = happyGoto action_54
action_196 (68) = happyGoto action_81
action_196 (72) = happyGoto action_263
action_196 _ = happyFail

action_197 (101) = happyShift action_158
action_197 _ = happyFail

action_198 (84) = happyShift action_82
action_198 (86) = happyShift action_83
action_198 (100) = happyShift action_84
action_198 (102) = happyShift action_59
action_198 (103) = happyShift action_85
action_198 (106) = happyShift action_86
action_198 (107) = happyShift action_87
action_198 (108) = happyShift action_88
action_198 (113) = happyShift action_89
action_198 (115) = happyShift action_90
action_198 (117) = happyShift action_91
action_198 (120) = happyShift action_92
action_198 (33) = happyGoto action_262
action_198 (34) = happyGoto action_74
action_198 (35) = happyGoto action_75
action_198 (36) = happyGoto action_76
action_198 (37) = happyGoto action_77
action_198 (48) = happyGoto action_78
action_198 (49) = happyGoto action_50
action_198 (52) = happyGoto action_79
action_198 (53) = happyGoto action_80
action_198 (55) = happyGoto action_53
action_198 (66) = happyGoto action_54
action_198 (68) = happyGoto action_81
action_198 _ = happyFail

action_199 (94) = happyShift action_261
action_199 (77) = happyGoto action_260
action_199 _ = happyReduce_154

action_200 _ = happyReduce_21

action_201 (97) = happyShift action_63
action_201 (32) = happyGoto action_259
action_201 _ = happyFail

action_202 (88) = happyShift action_258
action_202 _ = happyFail

action_203 (84) = happyShift action_82
action_203 (86) = happyShift action_83
action_203 (100) = happyShift action_84
action_203 (102) = happyShift action_59
action_203 (103) = happyShift action_85
action_203 (106) = happyShift action_86
action_203 (107) = happyShift action_87
action_203 (108) = happyShift action_88
action_203 (113) = happyShift action_89
action_203 (115) = happyShift action_90
action_203 (117) = happyShift action_91
action_203 (120) = happyShift action_92
action_203 (33) = happyGoto action_257
action_203 (34) = happyGoto action_74
action_203 (35) = happyGoto action_75
action_203 (36) = happyGoto action_76
action_203 (37) = happyGoto action_77
action_203 (48) = happyGoto action_78
action_203 (49) = happyGoto action_50
action_203 (52) = happyGoto action_79
action_203 (53) = happyGoto action_80
action_203 (55) = happyGoto action_53
action_203 (66) = happyGoto action_54
action_203 (68) = happyGoto action_81
action_203 _ = happyFail

action_204 (94) = happyShift action_256
action_204 _ = happyFail

action_205 (84) = happyShift action_82
action_205 (86) = happyShift action_208
action_205 (89) = happyShift action_255
action_205 (100) = happyShift action_84
action_205 (102) = happyShift action_59
action_205 (103) = happyShift action_209
action_205 (106) = happyShift action_86
action_205 (107) = happyShift action_87
action_205 (108) = happyShift action_88
action_205 (113) = happyShift action_210
action_205 (115) = happyShift action_90
action_205 (117) = happyShift action_91
action_205 (120) = happyShift action_92
action_205 (33) = happyGoto action_204
action_205 (34) = happyGoto action_74
action_205 (35) = happyGoto action_75
action_205 (36) = happyGoto action_76
action_205 (37) = happyGoto action_77
action_205 (41) = happyGoto action_254
action_205 (48) = happyGoto action_78
action_205 (49) = happyGoto action_50
action_205 (50) = happyGoto action_207
action_205 (51) = happyGoto action_52
action_205 (52) = happyGoto action_79
action_205 (53) = happyGoto action_80
action_205 (55) = happyGoto action_53
action_205 (66) = happyGoto action_54
action_205 (68) = happyGoto action_81
action_205 _ = happyFail

action_206 _ = happyReduce_82

action_207 (91) = happyShift action_253
action_207 _ = happyFail

action_208 (84) = happyShift action_82
action_208 (86) = happyShift action_252
action_208 (87) = happyShift action_105
action_208 (93) = happyShift action_106
action_208 (95) = happyShift action_45
action_208 (98) = happyShift action_107
action_208 (100) = happyShift action_84
action_208 (102) = happyShift action_59
action_208 (103) = happyShift action_209
action_208 (104) = happyShift action_46
action_208 (105) = happyShift action_108
action_208 (106) = happyShift action_86
action_208 (107) = happyShift action_87
action_208 (108) = happyShift action_88
action_208 (113) = happyShift action_89
action_208 (115) = happyShift action_90
action_208 (117) = happyShift action_91
action_208 (120) = happyShift action_92
action_208 (33) = happyGoto action_171
action_208 (34) = happyGoto action_74
action_208 (35) = happyGoto action_75
action_208 (36) = happyGoto action_76
action_208 (37) = happyGoto action_77
action_208 (48) = happyGoto action_78
action_208 (49) = happyGoto action_100
action_208 (51) = happyGoto action_197
action_208 (52) = happyGoto action_79
action_208 (53) = happyGoto action_172
action_208 (55) = happyGoto action_53
action_208 (57) = happyGoto action_173
action_208 (58) = happyGoto action_251
action_208 (60) = happyGoto action_102
action_208 (61) = happyGoto action_103
action_208 (66) = happyGoto action_54
action_208 (68) = happyGoto action_81
action_208 (69) = happyGoto action_104
action_208 _ = happyFail

action_209 (91) = happyReduce_111
action_209 (101) = happyReduce_136
action_209 _ = happyReduce_136

action_210 (84) = happyShift action_56
action_210 (86) = happyShift action_57
action_210 (88) = happyShift action_168
action_210 (102) = happyShift action_59
action_210 (103) = happyShift action_20
action_210 (106) = happyShift action_60
action_210 (107) = happyShift action_61
action_210 (15) = happyGoto action_250
action_210 (44) = happyGoto action_201
action_210 (45) = happyGoto action_97
action_210 (47) = happyGoto action_98
action_210 (48) = happyGoto action_99
action_210 (49) = happyGoto action_50
action_210 (50) = happyGoto action_51
action_210 (51) = happyGoto action_52
action_210 (55) = happyGoto action_53
action_210 (66) = happyGoto action_54
action_210 _ = happyFail

action_211 (87) = happyShift action_249
action_211 (93) = happyShift action_155
action_211 _ = happyFail

action_212 _ = happyReduce_91

action_213 (87) = happyShift action_248
action_213 _ = happyFail

action_214 (87) = happyShift action_247
action_214 _ = happyFail

action_215 _ = happyReduce_146

action_216 (96) = happyShift action_246
action_216 _ = happyReduce_159

action_217 _ = happyReduce_50

action_218 (84) = happyShift action_35
action_218 (86) = happyShift action_36
action_218 (88) = happyShift action_245
action_218 (99) = happyShift action_37
action_218 (102) = happyShift action_43
action_218 (103) = happyShift action_39
action_218 (21) = happyGoto action_132
action_218 (24) = happyGoto action_28
action_218 (62) = happyGoto action_31
action_218 (63) = happyGoto action_32
action_218 (76) = happyGoto action_243
action_218 (79) = happyGoto action_244
action_218 _ = happyReduce_158

action_219 (123) = happyShift action_242
action_219 (10) = happyGoto action_240
action_219 (81) = happyGoto action_241
action_219 _ = happyReduce_162

action_220 _ = happyReduce_117

action_221 (86) = happyShift action_19
action_221 (103) = happyShift action_20
action_221 (12) = happyGoto action_236
action_221 (13) = happyGoto action_237
action_221 (18) = happyGoto action_238
action_221 (51) = happyGoto action_239
action_221 _ = happyFail

action_222 (86) = happyShift action_122
action_222 (103) = happyShift action_39
action_222 (106) = happyShift action_123
action_222 (23) = happyGoto action_235
action_222 (63) = happyGoto action_121
action_222 _ = happyFail

action_223 _ = happyReduce_42

action_224 _ = happyReduce_30

action_225 _ = happyReduce_142

action_226 (94) = happyShift action_234
action_226 (77) = happyGoto action_233
action_226 _ = happyReduce_154

action_227 _ = happyReduce_24

action_228 (97) = happyShift action_63
action_228 (32) = happyGoto action_232
action_228 _ = happyFail

action_229 (84) = happyShift action_56
action_229 (86) = happyShift action_57
action_229 (102) = happyShift action_59
action_229 (103) = happyShift action_20
action_229 (106) = happyShift action_60
action_229 (107) = happyShift action_61
action_229 (46) = happyGoto action_47
action_229 (47) = happyGoto action_48
action_229 (48) = happyGoto action_49
action_229 (49) = happyGoto action_50
action_229 (50) = happyGoto action_51
action_229 (51) = happyGoto action_52
action_229 (55) = happyGoto action_53
action_229 (66) = happyGoto action_54
action_229 (78) = happyGoto action_55
action_229 _ = happyReduce_156

action_230 (86) = happyShift action_19
action_230 (103) = happyShift action_20
action_230 (16) = happyGoto action_231
action_230 (17) = happyGoto action_227
action_230 (31) = happyGoto action_228
action_230 (51) = happyGoto action_229
action_230 _ = happyFail

action_231 (94) = happyShift action_234
action_231 (77) = happyGoto action_290
action_231 _ = happyReduce_154

action_232 _ = happyReduce_26

action_233 (89) = happyShift action_289
action_233 _ = happyFail

action_234 (86) = happyShift action_19
action_234 (103) = happyShift action_20
action_234 (17) = happyGoto action_288
action_234 (31) = happyGoto action_228
action_234 (51) = happyGoto action_229
action_234 _ = happyReduce_153

action_235 _ = happyReduce_39

action_236 (94) = happyShift action_287
action_236 (77) = happyGoto action_286
action_236 _ = happyReduce_154

action_237 _ = happyReduce_18

action_238 _ = happyReduce_20

action_239 (101) = happyShift action_58
action_239 _ = happyFail

action_240 _ = happyReduce_161

action_241 _ = happyReduce_10

action_242 (86) = happyShift action_285
action_242 _ = happyFail

action_243 (84) = happyShift action_35
action_243 (86) = happyShift action_36
action_243 (99) = happyShift action_37
action_243 (102) = happyShift action_43
action_243 (103) = happyShift action_39
action_243 (21) = happyGoto action_140
action_243 (24) = happyGoto action_28
action_243 (62) = happyGoto action_31
action_243 (63) = happyGoto action_32
action_243 _ = happyReduce_157

action_244 _ = happyReduce_52

action_245 (86) = happyShift action_19
action_245 (103) = happyShift action_20
action_245 (29) = happyGoto action_282
action_245 (30) = happyGoto action_283
action_245 (51) = happyGoto action_284
action_245 _ = happyFail

action_246 (102) = happyShift action_220
action_246 (28) = happyGoto action_281
action_246 (54) = happyGoto action_218
action_246 _ = happyFail

action_247 _ = happyReduce_109

action_248 _ = happyReduce_103

action_249 _ = happyReduce_101

action_250 (94) = happyShift action_280
action_250 _ = happyFail

action_251 (87) = happyShift action_113
action_251 _ = happyFail

action_252 (84) = happyShift action_82
action_252 (86) = happyShift action_83
action_252 (87) = happyShift action_105
action_252 (93) = happyShift action_106
action_252 (95) = happyShift action_45
action_252 (98) = happyShift action_107
action_252 (100) = happyShift action_84
action_252 (102) = happyShift action_59
action_252 (103) = happyShift action_85
action_252 (104) = happyShift action_46
action_252 (105) = happyShift action_108
action_252 (106) = happyShift action_86
action_252 (107) = happyShift action_87
action_252 (108) = happyShift action_88
action_252 (113) = happyShift action_89
action_252 (115) = happyShift action_90
action_252 (117) = happyShift action_91
action_252 (120) = happyShift action_92
action_252 (33) = happyGoto action_171
action_252 (34) = happyGoto action_74
action_252 (35) = happyGoto action_75
action_252 (36) = happyGoto action_76
action_252 (37) = happyGoto action_77
action_252 (48) = happyGoto action_78
action_252 (49) = happyGoto action_100
action_252 (52) = happyGoto action_79
action_252 (53) = happyGoto action_172
action_252 (55) = happyGoto action_53
action_252 (57) = happyGoto action_173
action_252 (58) = happyGoto action_251
action_252 (60) = happyGoto action_102
action_252 (61) = happyGoto action_103
action_252 (66) = happyGoto action_54
action_252 (68) = happyGoto action_81
action_252 (69) = happyGoto action_104
action_252 _ = happyFail

action_253 (84) = happyShift action_82
action_253 (86) = happyShift action_83
action_253 (100) = happyShift action_84
action_253 (102) = happyShift action_59
action_253 (103) = happyShift action_85
action_253 (106) = happyShift action_86
action_253 (107) = happyShift action_87
action_253 (108) = happyShift action_88
action_253 (113) = happyShift action_89
action_253 (115) = happyShift action_90
action_253 (117) = happyShift action_91
action_253 (120) = happyShift action_92
action_253 (33) = happyGoto action_279
action_253 (34) = happyGoto action_74
action_253 (35) = happyGoto action_75
action_253 (36) = happyGoto action_76
action_253 (37) = happyGoto action_77
action_253 (48) = happyGoto action_78
action_253 (49) = happyGoto action_50
action_253 (52) = happyGoto action_79
action_253 (53) = happyGoto action_80
action_253 (55) = happyGoto action_53
action_253 (66) = happyGoto action_54
action_253 (68) = happyGoto action_81
action_253 _ = happyFail

action_254 _ = happyReduce_83

action_255 _ = happyReduce_65

action_256 _ = happyReduce_85

action_257 (119) = happyShift action_278
action_257 _ = happyFail

action_258 (84) = happyShift action_56
action_258 (86) = happyShift action_57
action_258 (102) = happyShift action_59
action_258 (103) = happyShift action_20
action_258 (106) = happyShift action_60
action_258 (107) = happyShift action_61
action_258 (38) = happyGoto action_275
action_258 (39) = happyGoto action_276
action_258 (44) = happyGoto action_277
action_258 (45) = happyGoto action_97
action_258 (47) = happyGoto action_98
action_258 (48) = happyGoto action_99
action_258 (49) = happyGoto action_50
action_258 (50) = happyGoto action_51
action_258 (51) = happyGoto action_52
action_258 (55) = happyGoto action_53
action_258 (66) = happyGoto action_54
action_258 _ = happyFail

action_259 _ = happyReduce_23

action_260 (89) = happyShift action_274
action_260 _ = happyFail

action_261 (84) = happyShift action_56
action_261 (86) = happyShift action_57
action_261 (102) = happyShift action_59
action_261 (103) = happyShift action_20
action_261 (106) = happyShift action_60
action_261 (107) = happyShift action_61
action_261 (15) = happyGoto action_273
action_261 (44) = happyGoto action_201
action_261 (45) = happyGoto action_97
action_261 (47) = happyGoto action_98
action_261 (48) = happyGoto action_99
action_261 (49) = happyGoto action_50
action_261 (50) = happyGoto action_51
action_261 (51) = happyGoto action_52
action_261 (55) = happyGoto action_53
action_261 (66) = happyGoto action_54
action_261 _ = happyReduce_153

action_262 _ = happyReduce_61

action_263 (87) = happyShift action_272
action_263 (93) = happyShift action_192
action_263 _ = happyFail

action_264 (87) = happyShift action_271
action_264 _ = happyFail

action_265 _ = happyReduce_144

action_266 _ = happyReduce_75

action_267 (84) = happyShift action_82
action_267 (86) = happyShift action_83
action_267 (100) = happyShift action_84
action_267 (102) = happyShift action_59
action_267 (103) = happyShift action_85
action_267 (106) = happyShift action_86
action_267 (107) = happyShift action_87
action_267 (108) = happyShift action_88
action_267 (113) = happyShift action_89
action_267 (115) = happyShift action_90
action_267 (117) = happyShift action_91
action_267 (120) = happyShift action_92
action_267 (33) = happyGoto action_270
action_267 (34) = happyGoto action_74
action_267 (35) = happyGoto action_75
action_267 (36) = happyGoto action_76
action_267 (37) = happyGoto action_77
action_267 (48) = happyGoto action_78
action_267 (49) = happyGoto action_50
action_267 (52) = happyGoto action_79
action_267 (53) = happyGoto action_80
action_267 (55) = happyGoto action_53
action_267 (66) = happyGoto action_54
action_267 (68) = happyGoto action_81
action_267 _ = happyFail

action_268 (86) = happyShift action_190
action_268 (103) = happyShift action_85
action_268 (43) = happyGoto action_269
action_268 (53) = happyGoto action_188
action_268 (68) = happyGoto action_81
action_268 _ = happyFail

action_269 _ = happyReduce_88

action_270 _ = happyReduce_89

action_271 _ = happyReduce_113

action_272 _ = happyReduce_73

action_273 _ = happyReduce_22

action_274 (114) = happyShift action_304
action_274 _ = happyFail

action_275 (94) = happyShift action_303
action_275 (77) = happyGoto action_302
action_275 _ = happyReduce_154

action_276 _ = happyReduce_79

action_277 (90) = happyShift action_301
action_277 _ = happyFail

action_278 (84) = happyShift action_82
action_278 (86) = happyShift action_83
action_278 (100) = happyShift action_84
action_278 (102) = happyShift action_59
action_278 (103) = happyShift action_85
action_278 (106) = happyShift action_86
action_278 (107) = happyShift action_87
action_278 (108) = happyShift action_88
action_278 (113) = happyShift action_89
action_278 (115) = happyShift action_90
action_278 (117) = happyShift action_91
action_278 (120) = happyShift action_92
action_278 (33) = happyGoto action_300
action_278 (34) = happyGoto action_74
action_278 (35) = happyGoto action_75
action_278 (36) = happyGoto action_76
action_278 (37) = happyGoto action_77
action_278 (48) = happyGoto action_78
action_278 (49) = happyGoto action_50
action_278 (52) = happyGoto action_79
action_278 (53) = happyGoto action_80
action_278 (55) = happyGoto action_53
action_278 (66) = happyGoto action_54
action_278 (68) = happyGoto action_81
action_278 _ = happyFail

action_279 (94) = happyShift action_299
action_279 _ = happyFail

action_280 _ = happyReduce_86

action_281 _ = happyReduce_51

action_282 (89) = happyShift action_297
action_282 (93) = happyShift action_298
action_282 _ = happyFail

action_283 _ = happyReduce_54

action_284 (101) = happyShift action_296
action_284 _ = happyFail

action_285 (102) = happyShift action_41
action_285 (64) = happyGoto action_33
action_285 (65) = happyGoto action_294
action_285 (70) = happyGoto action_295
action_285 _ = happyFail

action_286 (89) = happyShift action_293
action_286 _ = happyFail

action_287 (86) = happyShift action_19
action_287 (103) = happyShift action_20
action_287 (13) = happyGoto action_292
action_287 (18) = happyGoto action_238
action_287 (51) = happyGoto action_239
action_287 _ = happyReduce_153

action_288 _ = happyReduce_25

action_289 _ = happyReduce_12

action_290 (89) = happyShift action_291
action_290 _ = happyFail

action_291 _ = happyReduce_13

action_292 _ = happyReduce_19

action_293 _ = happyReduce_11

action_294 _ = happyReduce_139

action_295 (87) = happyShift action_311
action_295 (93) = happyShift action_312
action_295 _ = happyFail

action_296 (84) = happyShift action_35
action_296 (86) = happyShift action_36
action_296 (99) = happyShift action_37
action_296 (102) = happyShift action_43
action_296 (103) = happyShift action_39
action_296 (19) = happyGoto action_310
action_296 (20) = happyGoto action_125
action_296 (21) = happyGoto action_27
action_296 (24) = happyGoto action_28
action_296 (62) = happyGoto action_31
action_296 (63) = happyGoto action_32
action_296 _ = happyFail

action_297 _ = happyReduce_53

action_298 (86) = happyShift action_19
action_298 (103) = happyShift action_20
action_298 (30) = happyGoto action_309
action_298 (51) = happyGoto action_284
action_298 _ = happyFail

action_299 _ = happyReduce_84

action_300 _ = happyReduce_63

action_301 (84) = happyShift action_82
action_301 (86) = happyShift action_83
action_301 (100) = happyShift action_84
action_301 (102) = happyShift action_59
action_301 (103) = happyShift action_85
action_301 (106) = happyShift action_86
action_301 (107) = happyShift action_87
action_301 (108) = happyShift action_88
action_301 (113) = happyShift action_89
action_301 (115) = happyShift action_90
action_301 (117) = happyShift action_91
action_301 (120) = happyShift action_92
action_301 (33) = happyGoto action_308
action_301 (34) = happyGoto action_74
action_301 (35) = happyGoto action_75
action_301 (36) = happyGoto action_76
action_301 (37) = happyGoto action_77
action_301 (48) = happyGoto action_78
action_301 (49) = happyGoto action_50
action_301 (52) = happyGoto action_79
action_301 (53) = happyGoto action_80
action_301 (55) = happyGoto action_53
action_301 (66) = happyGoto action_54
action_301 (68) = happyGoto action_81
action_301 _ = happyFail

action_302 (89) = happyShift action_307
action_302 _ = happyFail

action_303 (84) = happyShift action_56
action_303 (86) = happyShift action_57
action_303 (102) = happyShift action_59
action_303 (103) = happyShift action_20
action_303 (106) = happyShift action_60
action_303 (107) = happyShift action_61
action_303 (39) = happyGoto action_306
action_303 (44) = happyGoto action_277
action_303 (45) = happyGoto action_97
action_303 (47) = happyGoto action_98
action_303 (48) = happyGoto action_99
action_303 (49) = happyGoto action_50
action_303 (50) = happyGoto action_51
action_303 (51) = happyGoto action_52
action_303 (55) = happyGoto action_53
action_303 (66) = happyGoto action_54
action_303 _ = happyReduce_153

action_304 (84) = happyShift action_82
action_304 (86) = happyShift action_83
action_304 (100) = happyShift action_84
action_304 (102) = happyShift action_59
action_304 (103) = happyShift action_85
action_304 (106) = happyShift action_86
action_304 (107) = happyShift action_87
action_304 (108) = happyShift action_88
action_304 (113) = happyShift action_89
action_304 (115) = happyShift action_90
action_304 (117) = happyShift action_91
action_304 (120) = happyShift action_92
action_304 (33) = happyGoto action_305
action_304 (34) = happyGoto action_74
action_304 (35) = happyGoto action_75
action_304 (36) = happyGoto action_76
action_304 (37) = happyGoto action_77
action_304 (48) = happyGoto action_78
action_304 (49) = happyGoto action_50
action_304 (52) = happyGoto action_79
action_304 (53) = happyGoto action_80
action_304 (55) = happyGoto action_53
action_304 (66) = happyGoto action_54
action_304 (68) = happyGoto action_81
action_304 _ = happyFail

action_305 _ = happyReduce_62

action_306 _ = happyReduce_80

action_307 _ = happyReduce_64

action_308 _ = happyReduce_81

action_309 _ = happyReduce_55

action_310 _ = happyReduce_56

action_311 _ = happyReduce_15

action_312 (102) = happyShift action_41
action_312 (64) = happyGoto action_33
action_312 (65) = happyGoto action_313
action_312 _ = happyFail

action_313 _ = happyReduce_140

happyReduce_1 = happyReduce 4 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Module (name happy_var_2) (fst happy_var_4) (snd happy_var_4)
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 6 5 happyReduction_2
happyReduction_2 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 ((happy_var_2, coalesce happy_var_4)
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 4 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 ((happy_var_2, [])
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 4 5 happyReduction_4
happyReduction_4 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (([], coalesce happy_var_2)
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  7 happyReduction_7
happyReduction_7 (HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Import (name happy_var_2)
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 ++ happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 6 9 happyReduction_10
happyReduction_10 ((HappyAbsSyn81  happy_var_6) `HappyStk`
	(HappyAbsSyn80  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn83  happy_var_3) `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (let { tyvars = fromMaybe [] happy_var_3;
            constrs = fromMaybe [] happy_var_5;
            derives = fromMaybe [] happy_var_6;
      } in [PDec ds | ds <- recordD (name happy_var_2) tyvars constrs derives]
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 8 9 happyReduction_11
happyReduction_11 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn75  happy_var_3) `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ([PDec (ClassD (name happy_var_2) happy_var_3 happy_var_6)]
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 7 9 happyReduction_12
happyReduction_12 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ([PDec (InstD [] happy_var_2 (icoalesce happy_var_5))]
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 8 9 happyReduction_13
happyReduction_13 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_3) `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ([PDec (InstD happy_var_2 happy_var_3 (icoalesce happy_var_6))]
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happyReduce 4 10 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (happy_var_3
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_1  11 happyReduction_16
happyReduction_16 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (PSig happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  11 happyReduction_17
happyReduction_17 (HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn11
		 (PClause (fst happy_var_1) (Clause (snd happy_var_1) happy_var_2)
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  12 happyReduction_18
happyReduction_18 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  12 happyReduction_19
happyReduction_19 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  13 happyReduction_20
happyReduction_20 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  14 happyReduction_21
happyReduction_21 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  14 happyReduction_22
happyReduction_22 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  15 happyReduction_23
happyReduction_23 (HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn15
		 ((happy_var_1, happy_var_2)
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  16 happyReduction_24
happyReduction_24 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 ([happy_var_1]
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  16 happyReduction_25
happyReduction_25 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  17 happyReduction_26
happyReduction_26 (HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn17
		 ((fst happy_var_1, Clause (snd happy_var_1) happy_var_2)
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  18 happyReduction_27
happyReduction_27 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn13
		 (TopSig (name happy_var_1) [] happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happyReduce 4 18 happyReduction_28
happyReduction_28 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (TopSig (name happy_var_1) happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_1  19 happyReduction_29
happyReduction_29 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  19 happyReduction_30
happyReduction_30 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (AppT (AppT (ConT (name "->")) happy_var_1) happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  20 happyReduction_31
happyReduction_31 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  20 happyReduction_32
happyReduction_32 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (AppT happy_var_1 happy_var_2
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  21 happyReduction_33
happyReduction_33 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn19
		 (ConT (name happy_var_1)
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  21 happyReduction_34
happyReduction_34 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn19
		 (VarT (name happy_var_1)
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  21 happyReduction_35
happyReduction_35 _
	(HappyAbsSyn71  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (tupT happy_var_2
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  21 happyReduction_36
happyReduction_36 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (AppT (ConT (name "[]")) happy_var_2
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  21 happyReduction_37
happyReduction_37 (HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (NumT happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  22 happyReduction_38
happyReduction_38 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  22 happyReduction_39
happyReduction_39 (HappyAbsSyn22  happy_var_3)
	(HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (AppNT happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  23 happyReduction_40
happyReduction_40 (HappyTerminal (TokenInteger happy_var_1))
	 =  HappyAbsSyn22
		 (ConNT happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  23 happyReduction_41
happyReduction_41 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn22
		 (VarNT (name happy_var_1)
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  23 happyReduction_42
happyReduction_42 _
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (happy_var_2
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  24 happyReduction_43
happyReduction_43 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_2  24 happyReduction_44
happyReduction_44 _
	_
	 =  HappyAbsSyn24
		 ("()"
	)

happyReduce_45 = happySpecReduce_2  24 happyReduction_45
happyReduction_45 _
	_
	 =  HappyAbsSyn24
		 ("[]"
	)

happyReduce_46 = happySpecReduce_3  24 happyReduction_46
happyReduction_46 _
	_
	_
	 =  HappyAbsSyn24
		 ("->"
	)

happyReduce_47 = happySpecReduce_3  24 happyReduction_47
happyReduction_47 _
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn24
		 ("(" ++ happy_var_2 ++ ")"
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happyMonadReduce 2 25 happyReduction_48
happyReduction_48 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( case attempt $ mkContext happy_var_1 of
         Right x -> return x
         Left msg -> lfailE msg)
	) (\r -> happyReturn (HappyAbsSyn25 r))

happyReduce_49 = happySpecReduce_2  26 happyReduction_49
happyReduction_49 (HappyAbsSyn71  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn26
		 (Class (name happy_var_1) happy_var_2
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  27 happyReduction_50
happyReduction_50 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 ([happy_var_1]
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  27 happyReduction_51
happyReduction_51 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_2  28 happyReduction_52
happyReduction_52 (HappyAbsSyn79  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn28
		 (NormalC (name happy_var_1) (fromMaybe [] happy_var_2)
	)
happyReduction_52 _ _  = notHappyAtAll 

happyReduce_53 = happyReduce 4 28 happyReduction_53
happyReduction_53 (_ `HappyStk`
	(HappyAbsSyn29  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (RecordC (name happy_var_1) happy_var_3
	) `HappyStk` happyRest

happyReduce_54 = happySpecReduce_1  29 happyReduction_54
happyReduction_54 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn29
		 ([happy_var_1]
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  29 happyReduction_55
happyReduction_55 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  30 happyReduction_56
happyReduction_56 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn30
		 ((name happy_var_1, happy_var_3)
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_2  31 happyReduction_57
happyReduction_57 (HappyAbsSyn78  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn31
		 ((name happy_var_1, fromMaybe [] happy_var_2)
	)
happyReduction_57 _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_2  32 happyReduction_58
happyReduction_58 (HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn32
		 (happy_var_2
	)
happyReduction_58 _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  33 happyReduction_59
happyReduction_59 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  33 happyReduction_60
happyReduction_60 (HappyAbsSyn32  happy_var_3)
	(HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (AppE (AppE happy_var_2 happy_var_1) happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happyReduce 4 34 happyReduction_61
happyReduction_61 ((HappyAbsSyn32  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn32
		 (LamE happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_62 = happyReduce 7 34 happyReduction_62
happyReduction_62 ((HappyAbsSyn32  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn32
		 (letE happy_var_3 happy_var_7
	) `HappyStk` happyRest

happyReduce_63 = happyReduce 6 34 happyReduction_63
happyReduction_63 ((HappyAbsSyn32  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn32  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn32  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn32
		 (ifE happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_64 = happyReduce 7 34 happyReduction_64
happyReduction_64 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn38  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn32  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn32
		 (CaseE happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_65 = happyMonadReduce 4 34 happyReduction_65
happyReduction_65 (_ `HappyStk`
	(HappyAbsSyn40  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( case last happy_var_3 of
         NoBindS _ -> return $ doE happy_var_3
         _ -> lfailE "last statement in do must be an expression")
	) (\r -> happyReturn (HappyAbsSyn32 r))

happyReduce_66 = happySpecReduce_1  34 happyReduction_66
happyReduction_66 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  35 happyReduction_67
happyReduction_67 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_2  35 happyReduction_68
happyReduction_68 (HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (AppE happy_var_1 happy_var_2
	)
happyReduction_68 _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  36 happyReduction_69
happyReduction_69 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  36 happyReduction_70
happyReduction_70 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn32
		 (ConE happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  36 happyReduction_71
happyReduction_71 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  36 happyReduction_72
happyReduction_72 _
	(HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn32
		 (happy_var_2
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happyReduce 5 36 happyReduction_73
happyReduction_73 (_ `HappyStk`
	(HappyAbsSyn72  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn32  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn32
		 (tupE (happy_var_2 : happy_var_4)
	) `HappyStk` happyRest

happyReduce_74 = happySpecReduce_3  36 happyReduction_74
happyReduction_74 _
	(HappyAbsSyn72  happy_var_2)
	_
	 =  HappyAbsSyn32
		 (listE happy_var_2
	)
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happyReduce 4 36 happyReduction_75
happyReduction_75 (_ `HappyStk`
	(HappyAbsSyn82  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn32  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn32
		 (case happy_var_1 of
        ConE s -> recordC s (fromMaybe [] happy_var_3)
        x -> recordU x (fromMaybe [] happy_var_3)
	) `HappyStk` happyRest

happyReduce_76 = happySpecReduce_1  37 happyReduction_76
happyReduction_76 (HappyTerminal (TokenInteger happy_var_1))
	 =  HappyAbsSyn32
		 (numberE happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  37 happyReduction_77
happyReduction_77 (HappyTerminal (TokenChar happy_var_1))
	 =  HappyAbsSyn32
		 (charE happy_var_1
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  37 happyReduction_78
happyReduction_78 (HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn32
		 (stringE happy_var_1
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_1  38 happyReduction_79
happyReduction_79 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn38
		 ([happy_var_1]
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_3  38 happyReduction_80
happyReduction_80 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_3  39 happyReduction_81
happyReduction_81 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn39
		 (Match happy_var_1 happy_var_3
	)
happyReduction_81 _ _ _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  40 happyReduction_82
happyReduction_82 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn40
		 ([happy_var_1]
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_2  40 happyReduction_83
happyReduction_83 (HappyAbsSyn41  happy_var_2)
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_83 _ _  = notHappyAtAll 

happyReduce_84 = happyReduce 4 41 happyReduction_84
happyReduction_84 (_ `HappyStk`
	(HappyAbsSyn32  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn41
		 (BindS (VarP happy_var_1) happy_var_3
	) `HappyStk` happyRest

happyReduce_85 = happySpecReduce_2  41 happyReduction_85
happyReduction_85 _
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn41
		 (NoBindS happy_var_1
	)
happyReduction_85 _ _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_3  41 happyReduction_86
happyReduction_86 _
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn41
		 (LetS (fst happy_var_2) (snd happy_var_2)
	)
happyReduction_86 _ _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_1  42 happyReduction_87
happyReduction_87 (HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn42
		 ([happy_var_1]
	)
happyReduction_87 _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_3  42 happyReduction_88
happyReduction_88 (HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_88 _ _ _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_3  43 happyReduction_89
happyReduction_89 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn43
		 ((name happy_var_1, happy_var_3)
	)
happyReduction_89 _ _ _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  44 happyReduction_90
happyReduction_90 (HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn44
		 (happy_var_1
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_3  44 happyReduction_91
happyReduction_91 (HappyAbsSyn44  happy_var_3)
	_
	(HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn44
		 (ConP UnknownT (name ":") [happy_var_1, happy_var_3]
	)
happyReduction_91 _ _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_2  45 happyReduction_92
happyReduction_92 (HappyAbsSyn46  happy_var_2)
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn44
		 (let Sig n t = happy_var_1 in ConP t n happy_var_2
	)
happyReduction_92 _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_1  45 happyReduction_93
happyReduction_93 (HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn44
		 (happy_var_1
	)
happyReduction_93 _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_1  46 happyReduction_94
happyReduction_94 (HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn46
		 ([happy_var_1]
	)
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_2  46 happyReduction_95
happyReduction_95 (HappyAbsSyn44  happy_var_2)
	(HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn46
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_95 _ _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_1  47 happyReduction_96
happyReduction_96 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn44
		 (let Sig n t = happy_var_1 in if n == (name "_") then WildP t else VarP happy_var_1
	)
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_1  47 happyReduction_97
happyReduction_97 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn44
		 (let Sig n t = happy_var_1 in ConP t n []
	)
happyReduction_97 _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_1  47 happyReduction_98
happyReduction_98 (HappyTerminal (TokenInteger happy_var_1))
	 =  HappyAbsSyn44
		 (LitP (IntegerL happy_var_1)
	)
happyReduction_98 _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_1  47 happyReduction_99
happyReduction_99 (HappyTerminal (TokenChar happy_var_1))
	 =  HappyAbsSyn44
		 (LitP (CharL happy_var_1)
	)
happyReduction_99 _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_3  47 happyReduction_100
happyReduction_100 _
	(HappyAbsSyn44  happy_var_2)
	_
	 =  HappyAbsSyn44
		 (happy_var_2
	)
happyReduction_100 _ _ _  = notHappyAtAll 

happyReduce_101 = happyReduce 5 47 happyReduction_101
happyReduction_101 (_ `HappyStk`
	(HappyAbsSyn46  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn44  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn44
		 (tupP (happy_var_2 : happy_var_4)
	) `HappyStk` happyRest

happyReduce_102 = happySpecReduce_3  47 happyReduction_102
happyReduction_102 _
	(HappyAbsSyn46  happy_var_2)
	_
	 =  HappyAbsSyn44
		 (listP happy_var_2
	)
happyReduction_102 _ _ _  = notHappyAtAll 

happyReduce_103 = happyReduce 5 48 happyReduction_103
happyReduction_103 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn48
		 (Sig (name happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_104 = happySpecReduce_1  48 happyReduction_104
happyReduction_104 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn48
		 (Sig (name happy_var_1) UnknownT
	)
happyReduction_104 _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_2  49 happyReduction_105
happyReduction_105 _
	_
	 =  HappyAbsSyn24
		 ("()"
	)

happyReduce_106 = happySpecReduce_2  49 happyReduction_106
happyReduction_106 _
	_
	 =  HappyAbsSyn24
		 ("[]"
	)

happyReduce_107 = happySpecReduce_3  49 happyReduction_107
happyReduction_107 _
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn24
		 ("(" ++ happy_var_2 ++ ")"
	)
happyReduction_107 _ _ _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_1  49 happyReduction_108
happyReduction_108 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_108 _  = notHappyAtAll 

happyReduce_109 = happyReduce 5 50 happyReduction_109
happyReduction_109 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn48
		 (Sig (name happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_110 = happySpecReduce_1  50 happyReduction_110
happyReduction_110 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn48
		 (Sig (name happy_var_1) UnknownT
	)
happyReduction_110 _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_1  51 happyReduction_111
happyReduction_111 (HappyTerminal (TokenVarId happy_var_1))
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_3  51 happyReduction_112
happyReduction_112 _
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (happy_var_2
	)
happyReduction_112 _ _ _  = notHappyAtAll 

happyReduce_113 = happyReduce 5 52 happyReduction_113
happyReduction_113 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn32
		 (VarE (Sig (name happy_var_2) happy_var_4)
	) `HappyStk` happyRest

happyReduce_114 = happySpecReduce_1  52 happyReduction_114
happyReduction_114 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn32
		 (VarE (Sig (name happy_var_1) UnknownT)
	)
happyReduction_114 _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_1  53 happyReduction_115
happyReduction_115 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_115 _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_3  53 happyReduction_116
happyReduction_116 _
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (happy_var_2
	)
happyReduction_116 _ _ _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_1  54 happyReduction_117
happyReduction_117 (HappyTerminal (TokenConId happy_var_1))
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_117 _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_1  55 happyReduction_118
happyReduction_118 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_118 _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_3  55 happyReduction_119
happyReduction_119 _
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (happy_var_2
	)
happyReduction_119 _ _ _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_1  56 happyReduction_120
happyReduction_120 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn32
		 (VarE (Sig (name happy_var_1) UnknownT)
	)
happyReduction_120 _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_1  56 happyReduction_121
happyReduction_121 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn32
		 (ConE (Sig (name happy_var_1) UnknownT)
	)
happyReduction_121 _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_1  57 happyReduction_122
happyReduction_122 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_122 _  = notHappyAtAll 

happyReduce_123 = happySpecReduce_1  58 happyReduction_123
happyReduction_123 (HappyTerminal (TokenVarSym happy_var_1))
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_123 _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_1  58 happyReduction_124
happyReduction_124 _
	 =  HappyAbsSyn24
		 ("."
	)

happyReduce_125 = happySpecReduce_1  59 happyReduction_125
happyReduction_125 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_125 _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_1  60 happyReduction_126
happyReduction_126 _
	 =  HappyAbsSyn24
		 (":"
	)

happyReduce_127 = happySpecReduce_1  60 happyReduction_127
happyReduction_127 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_127 _  = notHappyAtAll 

happyReduce_128 = happySpecReduce_1  61 happyReduction_128
happyReduction_128 (HappyTerminal (TokenConSym happy_var_1))
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_128 _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_1  62 happyReduction_129
happyReduction_129 (HappyTerminal (TokenConId happy_var_1))
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_129 _  = notHappyAtAll 

happyReduce_130 = happySpecReduce_1  63 happyReduction_130
happyReduction_130 (HappyTerminal (TokenVarId happy_var_1))
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_130 _  = notHappyAtAll 

happyReduce_131 = happySpecReduce_1  64 happyReduction_131
happyReduction_131 (HappyTerminal (TokenConId happy_var_1))
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_131 _  = notHappyAtAll 

happyReduce_132 = happySpecReduce_1  65 happyReduction_132
happyReduction_132 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_132 _  = notHappyAtAll 

happyReduce_133 = happySpecReduce_1  66 happyReduction_133
happyReduction_133 (HappyTerminal (TokenConId happy_var_1))
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_133 _  = notHappyAtAll 

happyReduce_134 = happySpecReduce_1  67 happyReduction_134
happyReduction_134 (HappyTerminal (TokenConId happy_var_1))
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_134 _  = notHappyAtAll 

happyReduce_135 = happySpecReduce_3  67 happyReduction_135
happyReduction_135 (HappyTerminal (TokenConId happy_var_3))
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1 ++ "." ++ happy_var_3
	)
happyReduction_135 _ _ _  = notHappyAtAll 

happyReduce_136 = happySpecReduce_1  68 happyReduction_136
happyReduction_136 (HappyTerminal (TokenVarId happy_var_1))
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_136 _  = notHappyAtAll 

happyReduce_137 = happySpecReduce_1  69 happyReduction_137
happyReduction_137 _
	 =  HappyAbsSyn24
		 (","
	)

happyReduce_138 = happySpecReduce_2  69 happyReduction_138
happyReduction_138 _
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (',':happy_var_1
	)
happyReduction_138 _ _  = notHappyAtAll 

happyReduce_139 = happySpecReduce_1  70 happyReduction_139
happyReduction_139 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_139 _  = notHappyAtAll 

happyReduce_140 = happySpecReduce_3  70 happyReduction_140
happyReduction_140 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_140 _ _ _  = notHappyAtAll 

happyReduce_141 = happySpecReduce_1  71 happyReduction_141
happyReduction_141 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn71
		 ([happy_var_1]
	)
happyReduction_141 _  = notHappyAtAll 

happyReduce_142 = happySpecReduce_3  71 happyReduction_142
happyReduction_142 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn71  happy_var_1)
	 =  HappyAbsSyn71
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_142 _ _ _  = notHappyAtAll 

happyReduce_143 = happySpecReduce_1  72 happyReduction_143
happyReduction_143 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn72
		 ([happy_var_1]
	)
happyReduction_143 _  = notHappyAtAll 

happyReduce_144 = happySpecReduce_3  72 happyReduction_144
happyReduction_144 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn72  happy_var_1)
	 =  HappyAbsSyn72
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_144 _ _ _  = notHappyAtAll 

happyReduce_145 = happySpecReduce_1  73 happyReduction_145
happyReduction_145 (HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn46
		 ([happy_var_1]
	)
happyReduction_145 _  = notHappyAtAll 

happyReduce_146 = happySpecReduce_3  73 happyReduction_146
happyReduction_146 (HappyAbsSyn44  happy_var_3)
	_
	(HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn46
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_146 _ _ _  = notHappyAtAll 

happyReduce_147 = happySpecReduce_1  74 happyReduction_147
happyReduction_147 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn74
		 (NormalTV (name happy_var_1)
	)
happyReduction_147 _  = notHappyAtAll 

happyReduce_148 = happySpecReduce_2  74 happyReduction_148
happyReduction_148 (HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn74
		 (NumericTV (name happy_var_2)
	)
happyReduction_148 _ _  = notHappyAtAll 

happyReduce_149 = happySpecReduce_1  75 happyReduction_149
happyReduction_149 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn75
		 ([happy_var_1]
	)
happyReduction_149 _  = notHappyAtAll 

happyReduce_150 = happySpecReduce_2  75 happyReduction_150
happyReduction_150 (HappyAbsSyn74  happy_var_2)
	(HappyAbsSyn75  happy_var_1)
	 =  HappyAbsSyn75
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_150 _ _  = notHappyAtAll 

happyReduce_151 = happySpecReduce_1  76 happyReduction_151
happyReduction_151 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn71
		 ([happy_var_1]
	)
happyReduction_151 _  = notHappyAtAll 

happyReduce_152 = happySpecReduce_2  76 happyReduction_152
happyReduction_152 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn71  happy_var_1)
	 =  HappyAbsSyn71
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_152 _ _  = notHappyAtAll 

happyReduce_153 = happySpecReduce_1  77 happyReduction_153
happyReduction_153 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn77
		 (Just happy_var_1
	)
happyReduction_153 _  = notHappyAtAll 

happyReduce_154 = happySpecReduce_0  77 happyReduction_154
happyReduction_154  =  HappyAbsSyn77
		 (Nothing
	)

happyReduce_155 = happySpecReduce_1  78 happyReduction_155
happyReduction_155 (HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn78
		 (Just happy_var_1
	)
happyReduction_155 _  = notHappyAtAll 

happyReduce_156 = happySpecReduce_0  78 happyReduction_156
happyReduction_156  =  HappyAbsSyn78
		 (Nothing
	)

happyReduce_157 = happySpecReduce_1  79 happyReduction_157
happyReduction_157 (HappyAbsSyn71  happy_var_1)
	 =  HappyAbsSyn79
		 (Just happy_var_1
	)
happyReduction_157 _  = notHappyAtAll 

happyReduce_158 = happySpecReduce_0  79 happyReduction_158
happyReduction_158  =  HappyAbsSyn79
		 (Nothing
	)

happyReduce_159 = happySpecReduce_1  80 happyReduction_159
happyReduction_159 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn80
		 (Just happy_var_1
	)
happyReduction_159 _  = notHappyAtAll 

happyReduce_160 = happySpecReduce_0  80 happyReduction_160
happyReduction_160  =  HappyAbsSyn80
		 (Nothing
	)

happyReduce_161 = happySpecReduce_1  81 happyReduction_161
happyReduction_161 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn81
		 (Just happy_var_1
	)
happyReduction_161 _  = notHappyAtAll 

happyReduce_162 = happySpecReduce_0  81 happyReduction_162
happyReduction_162  =  HappyAbsSyn81
		 (Nothing
	)

happyReduce_163 = happySpecReduce_1  82 happyReduction_163
happyReduction_163 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn82
		 (Just happy_var_1
	)
happyReduction_163 _  = notHappyAtAll 

happyReduce_164 = happySpecReduce_0  82 happyReduction_164
happyReduction_164  =  HappyAbsSyn82
		 (Nothing
	)

happyReduce_165 = happySpecReduce_1  83 happyReduction_165
happyReduction_165 (HappyAbsSyn75  happy_var_1)
	 =  HappyAbsSyn83
		 (Just happy_var_1
	)
happyReduction_165 _  = notHappyAtAll 

happyReduce_166 = happySpecReduce_0  83 happyReduction_166
happyReduction_166  =  HappyAbsSyn83
		 (Nothing
	)

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TokenEOF -> action 124 124 tk (HappyState action) sts stk;
	TokenOpenBracket -> cont 84;
	TokenCloseBracket -> cont 85;
	TokenOpenParen -> cont 86;
	TokenCloseParen -> cont 87;
	TokenOpenBrace -> cont 88;
	TokenCloseBrace -> cont 89;
	TokenDashArrow -> cont 90;
	TokenBindArrow -> cont 91;
	TokenEqualsArrow -> cont 92;
	TokenComma -> cont 93;
	TokenSemicolon -> cont 94;
	TokenPeriod -> cont 95;
	TokenBar -> cont 96;
	TokenEquals -> cont 97;
	TokenColon -> cont 98;
	TokenHash -> cont 99;
	TokenBackSlash -> cont 100;
	TokenDoubleColon -> cont 101;
	TokenConId happy_dollar_dollar -> cont 102;
	TokenVarId happy_dollar_dollar -> cont 103;
	TokenVarSym happy_dollar_dollar -> cont 104;
	TokenConSym happy_dollar_dollar -> cont 105;
	TokenInteger happy_dollar_dollar -> cont 106;
	TokenChar happy_dollar_dollar -> cont 107;
	TokenString happy_dollar_dollar -> cont 108;
	TokenData -> cont 109;
	TokenClass -> cont 110;
	TokenInstance -> cont 111;
	TokenWhere -> cont 112;
	TokenLet -> cont 113;
	TokenIn -> cont 114;
	TokenCase -> cont 115;
	TokenOf -> cont 116;
	TokenIf -> cont 117;
	TokenThen -> cont 118;
	TokenElse -> cont 119;
	TokenDo -> cont 120;
	TokenModule -> cont 121;
	TokenImport -> cont 122;
	TokenDeriving -> cont 123;
	_ -> happyError' tk
	})

happyError_ 124 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => ParserMonad a -> (a -> ParserMonad b) -> ParserMonad b
happyThen = (>>=)
happyReturn :: () => a -> ParserMonad a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> ParserMonad a
happyReturn1 = happyReturn
happyError' :: () => (Token) -> ParserMonad a
happyError' tk = parseError tk

seri_module = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: Token -> ParserMonad a
parseError tok = lfailE $ "parser error at " ++ show tok

data PDec =
    PDec Dec
  | PSig TopSig
  | PClause Name Clause

isPClause :: PDec -> Bool
isPClause (PClause {}) = True
isPClause _ = False

coalesce :: [PDec] -> [Dec]
coalesce [] = []
coalesce ((PSig s):ds) =
    let (ms, rds) = span isPClause ds
        rest = coalesce rds
        d = case ms of
                [] -> PrimD s
                _ -> ValD s (clauseE [c | PClause _ c <- ms]) 
    in (d:rest)
coalesce ((PDec d):ds) = d : coalesce ds

-- Merge clauses for the same method into a single method.
icoalesce :: [(Name, Clause)] -> [Method]
icoalesce [] = []
icoalesce ((n, c):ms) =
    let (me, rms) = span (\(n', _) -> n' == n) ms
        rest = icoalesce rms
        m = Method n (clauseE (c : map snd me))
    in (m : rest)

-- A context is parsed first as a type to avoid a reduce/reduce conflict. Here
-- we turn that type back into a proper context.
mkContext :: Type -> Failable [Class] 
mkContext t = 
  let mkclass :: Type -> Failable Class
      mkclass t =
        case unappsT t of
          (ConT nm):ts -> return $ Class nm ts
          _ -> fail $ "invalid context"

      classes = untupT t
  in mapM mkclass classes
      
      

parse :: FilePath -> String -> Failable Module
parse = runParser seri_module
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
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

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

{-# LINE 312 "templates/GenericTemplate.hs" #-}
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
