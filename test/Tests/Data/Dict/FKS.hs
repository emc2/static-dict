-- Copyright (c) 2016 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the author nor the names of any contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS''
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
-- USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

module Tests.Data.Dict.FKS(tests) where

import Data.Dict.FKS
import Data.Foldable
import Data.List(sort)
import Data.Maybe
import Data.Traversable
import Data.Vector(Vector, (!), fromList)
import Prelude hiding (lookup)
import Test.HUnitPlus.Base

import qualified Data.Map as Map

randdata :: Vector Int
randdata = fromList [
  359, 3363, 5518, 5885, 6667, 8515, 9082, 10473,
  10486, 17474, 17962, 18238, 18770, 19899, 20871, 21951,
  24653, 25187, 26802, 28460, 30377, 31982, 32382, 32718,
  33667, 33861, 35128, 35945, 36489, 39580, 42823, 44857,
  45004, 45529, 46511, 47011, 47232, 48286, 48414, 48449,
  48556, 48575, 50315, 50344, 54525, 59140, 59391, 59672,
  59965, 61406, 62718, 62877, 63127, 65468, 67051, 67093,
  71843, 73351, 73798, 74216, 74383, 76650, 77733, 79125,
  79513, 81169, 81498, 82126, 83090, 83983, 85639, 87380,
  88219, 88565, 89563, 90320, 90419, 91273, 91281, 91698,
  95623, 99548, 100469, 105081, 106447, 107874, 109079, 109099,
  111040, 111400, 111633, 111776, 113786, 113984, 113994, 114824,
  116322, 116378, 116644, 117373, 121064, 121080, 121535, 122542,
  122564, 123523, 124685, 124847, 125130, 126316, 132671, 133759,
  134855, 135331, 135437, 135778, 139420, 139665, 140619, 141215,
  141312, 141436, 141938, 141963, 144194, 146103, 146464, 146834,
  147884, 148532, 149606, 149616, 150985, 150988, 152492, 155777,
  161206, 161975, 162330, 163445, 165822, 166162, 166572, 167065,
  167529, 167776, 168535, 168726, 171936, 172185, 173514, 173752,
  174090, 176468, 177571, 178018, 178881, 179248, 180169, 181717,
  182474, 182712, 183351, 183731, 184054, 184867, 185257, 186623,
  187348, 187732, 188857, 189671, 189819, 190091, 191236, 193107,
  195018, 196584, 197538, 199534, 199675, 200669, 202184, 202316,
  202873, 203320, 203530, 203982, 205262, 205523, 207139, 207150,
  207994, 208328, 209979, 210224, 211628, 213152, 213233, 213395,
  215509, 216905, 217741, 217794, 219620, 220226, 220338, 220944,
  222429, 223737, 223944, 223946, 224482, 224679, 225291, 226463,
  227647, 227990, 229738, 229957, 230111, 230899, 231824, 232506,
  232708, 234205, 234702, 236823, 237399, 237599, 237670, 237917,
  239376, 241302, 242288, 244510, 246851, 249614, 250833, 251786,
  252608, 253645, 254189, 254516, 254587, 255245, 255268, 257220,
  257329, 257365, 258525, 260511, 264370, 265801, 268714, 269244,
  269674, 274233, 276757, 281108, 281839, 286060, 286086, 287091,
  287353, 289095, 290586, 290823, 292303, 293413, 297511, 297763,
  299311, 299919, 301493, 301552, 302600, 302653, 302957, 303859,
  306227, 307694, 309804, 311025, 311908, 312892, 314034, 315139,
  315579, 316434, 317814, 319678, 319991, 324802, 324813, 325288,
  328566, 328863, 329680, 331881, 332978, 333107, 333174, 333229,
  335306, 335613, 337132, 338843, 339738, 341702, 342763, 343796,
  344371, 345173, 346856, 347002, 347798, 349109, 350259, 350485,
  350543, 352163, 353533, 354017, 355398, 356305, 357190, 357339,
  357378, 360347, 362729, 364299, 364373, 366550, 367832, 368932,
  370245, 371988, 373636, 374397, 374607, 374972, 376369, 377338,
  378462, 382202, 383109, 383511, 384508, 385077, 385737, 385842,
  386850, 391201, 393090, 393530, 393661, 393966, 394863, 395607,
  396635, 396814, 397182, 399101, 399239, 401001, 401671, 401848,
  401871, 402963, 403390, 404183, 404627, 404796, 405512, 405640,
  410430, 410988, 411132, 411247, 412109, 412204, 412354, 412806,
  414976, 415457, 417378, 417732, 418536, 418810, 418892, 419805,
  420831, 421405, 421911, 423118, 423453, 426839, 427066, 428475,
  434487, 435023, 435034, 436672, 440017, 440206, 441041, 441246,
  442193, 442295, 443012, 444851, 445588, 446508, 448122, 448130,
  448446, 449448, 451167, 451416, 452630, 455731, 456736, 457386,
  459661, 461810, 463929, 470131, 470218, 470260, 470322, 471969,
  472431, 474031, 475192, 476028, 477314, 478174, 478797, 478938,
  479622, 481313, 482474, 483142, 483227, 483558, 483907, 484392,
  487751, 488786, 489889, 492708, 493114, 497247, 497444, 497586,
  499502, 500102, 500604, 500620, 501207, 504457, 507122, 507849,
  508907, 509241, 509340, 509431, 513403, 515837, 515968, 516015,
  516326, 520648, 521196, 521469, 524934, 525870, 527667, 528607,
  529367, 529430, 529785, 530658, 531042, 532475, 534949, 535422,
  535745, 536618, 538978, 539776, 539889, 542821, 544547, 545319,
  545533, 547151, 547733, 548895, 549237, 550857, 551284, 552048,
  554884, 556372, 556445, 556573, 557945, 558244, 559057, 559622,
  560075, 560123, 560637, 561228, 561918, 562408, 564266, 564343,
  564963, 565512, 566710, 566806, 567347, 567562, 568619, 568677,
  569665, 571343, 572830, 573241, 574081, 574461, 574987, 576064,
  576152, 578427, 579443, 580331, 581673, 581680, 582782, 583340,
  583781, 585821, 586108, 587080, 588103, 589657, 590699, 591616,
  591654, 592507, 593739, 594160, 595832, 596241, 597963, 598256,
  600041, 600727, 601218, 601245, 601467, 601677, 603613, 604391,
  604747, 605308, 605524, 606137, 606736, 606740, 607439, 608375,
  608402, 608546, 609098, 609998, 610715, 610940, 612719, 612951,
  613034, 613630, 613712, 613775, 614260, 614781, 614827, 616290,
  617169, 617758, 620969, 621635, 622810, 623044, 623142, 623329,
  623908, 624604, 626814, 627060, 627630, 628087, 628238, 628682,
  629084, 629816, 629871, 629910, 632236, 636686, 637859, 639196,
  642640, 642771, 645311, 645642, 646946, 647009, 647130, 648200,
  648307, 649334, 650576, 651331, 652950, 653461, 654106, 656302,
  657798, 657958, 658970, 660708, 663963, 664272, 664599, 672048,
  673192, 677446, 677609, 677651, 677961, 678050, 678686, 679335,
  680088, 680977, 681153, 681645, 681799, 682453, 683415, 683549,
  685010, 686965, 687519, 688480, 688774, 690046, 691101, 691149,
  691639, 692776, 693398, 693532, 693623, 697111, 697327, 697598,
  697747, 698555, 699704, 699823, 701076, 701660, 702176, 702841,
  705242, 706046, 706398, 707054, 709229, 709674, 709807, 710101,
  712032, 717525, 717698, 722720, 723752, 723927, 724819, 724893,
  729325, 730439, 731307, 731622, 732418, 732941, 733332, 734715,
  735441, 736721, 738022, 739049, 739842, 744178, 744921, 745817,
  745925, 747068, 747970, 749617, 750071, 750877, 751544, 753699,
  753713, 754639, 755592, 756813, 757075, 757637, 758057, 761062,
  762352, 762432, 767585, 768733, 769025, 769451, 770201, 772387,
  772460, 772528, 773587, 774160, 774598, 776953, 777011, 777857,
  778577, 779367, 779662, 780696, 784213, 789122, 789782, 791005,
  791186, 795682, 795844, 796049, 796180, 796384, 798670, 798682,
  799329, 804340, 805098, 805379, 805459, 806523, 807170, 807407,
  807908, 807931, 807936, 809772, 810219, 810418, 810847, 811247,
  812144, 812439, 812655, 813409, 813921, 815682, 816532, 816794,
  817227, 817600, 817769, 818553, 819183, 820187, 820199, 823689,
  824279, 824324, 825161, 825541, 826444, 826731, 826881, 827076,
  827894, 828645, 829904, 830877, 832255, 832324, 834336, 834961,
  835173, 836209, 838300, 838860, 839707, 839777, 841468, 842616,
  843168, 845313, 847880, 848227, 849168, 850548, 851217, 851367,
  852652, 853008, 854459, 855116, 856755, 857147, 857614, 857980,
  858208, 858509, 861792, 861830, 862027, 863569, 865897, 866430,
  866558, 868025, 868921, 870903, 871048, 871114, 871566, 871772,
  871784, 874788, 876317, 877278, 878130, 879301, 879499, 879699,
  880284, 883222, 884465, 884623, 884715, 887073, 887973, 888998,
  889646, 890501, 890930, 891264, 892406, 896638, 898082, 899713,
  901123, 901386, 905605, 906068, 906337, 906524, 906537, 907129,
  907540, 910915, 912034, 913305, 914109, 914461, 915245, 915451,
  915832, 917061, 917269, 918355, 918457, 918576, 918766, 919106,
  920138, 920537, 922347, 926498, 926555, 926865, 927224, 928095,
  930357, 931497, 932202, 932296, 936241, 937105, 937148, 937164,
  937177, 937767, 937959, 939045, 941736, 942228, 942668, 942735,
  943095, 944873, 945818, 945980, 946810, 948881, 948922, 948995,
  949419, 951691, 951832, 956913, 957057, 958044, 959211, 959893,
  960169, 961256, 963260, 964633, 965409, 966320, 966404, 969474,
  969785, 970283, 971681, 971699, 972614, 974640, 975257, 976288,
  976669, 977018, 977425, 980344, 981599, 982420, 982660, 984087,
  985204, 987231, 989511, 990694, 991132, 991647, 992228, 994554,
  994654, 996238, 996259, 996319, 999303, 1000165, 1000428, 1000724,
  1001554, 1003110, 1004302, 1005110, 1005271, 1011788, 1015114, 1017141,
  1017511, 1018628, 1019038, 1019871, 1020429, 1021434, 1021675, 1022458,
  1023080, 1023874, 1024881, 1026481, 1028034, 1028817, 1029934, 1030542,
  1031151, 1032281, 1032613, 1032788, 1033300, 1034037, 1034965, 1037066,
  1037593, 1038470, 1039434, 1039468, 1041599, 1043770, 1043807, 1044121,
  1044918, 1045631, 1045992, 1046172, 1046389, 1047023, 1047392, 1173953]

sizes :: [Int]
sizes = [ 1, 2, 3, 4, 5, 6, 7, 8,
          10, 12, 16, 20, 27, 31, 32, 33,
          48, 64, 100, 256, 384, 512, 800, 1024 ]

generators :: [(String, Int -> Int)]
generators = [
    ("direct", \n -> n),
    ("negative", \n -> -n),
    ("double", \n -> 2 * n),
    ("triple", \n -> 3 * n),
    ("oscillating", \n -> if even (n+1)
                          then (n+1) `div` 2
                          else - (n+1) `div` 2),
    ("neg_pos", \n -> if even n then n else -n),
    ("split", \n -> if even n then n * 2 else n),
    ("split_offset", \n -> if even n then n * 2 else n + 1000),
    ("random", \n -> randdata ! n) ]

checkdict :: [(Int, Int)] -> FKSDict Int Int -> IO ()
checkdict pairs d =
  let
    idxs = map fst pairs
    sortedelems = sort (map snd pairs)
    expected = Map.fromList pairs

    checkparity :: Int -> IO ()
    checkparity idx = lookup d idx @?= Map.lookup idx expected

    checkmember :: Int -> IO ()
    checkmember idx = isJust (lookup d idx) @=? member d idx
  in do
    expected @=? Map.fromList (assocs d)
    mapM_ checkparity ([0..2048] ++ idxs)
    mapM_ checkmember ([0..2048] ++ idxs)
    sortedelems @=? sort (foldr (:) [] d)

maketest :: (String, Int -> Int) -> Int -> Int -> Test
maketest (genname, gen) size num =
  let
    testname = "dict_" ++ genname ++ "_" ++ show size ++ "_" ++ show num
    pairs = map (\n -> (gen n, n)) [0..size - 1]

    inc (k, e) = (k, e + 1)
  in
    testname ~: do
      d <- dict pairs
      checkdict pairs d
      checkdict (fmap inc pairs) (fmap (+1) d)
      d' <- mapM (return . (+1)) d
      checkdict (fmap inc pairs) d'

testcases :: [Test]
testcases =
  foldr (\gen accum1 ->
          foldr (\size accum2 ->
                  foldr (\num accum3 ->
                          maketest gen size num : accum3)
                        accum2 [0..8])
                accum1 sizes)
        [] generators

tests :: Test
tests = "FKS" ~: testcases
