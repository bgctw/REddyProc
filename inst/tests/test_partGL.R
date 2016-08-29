#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for fConvertTimeToPosix functions +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: TW
#require(testthat)
context("partGL")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 8 first days of June from IT-MBo.2005.txt
dsNEE <- structure(list(sDateTime = structure(c(1117584900, 1117586700, 
								1117588500, 1117590300, 1117592100, 1117593900, 1117595700, 1117597500, 
								1117599300, 1117601100, 1117602900, 1117604700, 1117606500, 1117608300, 
								1117610100, 1117611900, 1117613700, 1117615500, 1117617300, 1117619100, 
								1117620900, 1117622700, 1117624500, 1117626300, 1117628100, 1117629900, 
								1117631700, 1117633500, 1117635300, 1117637100, 1117638900, 1117640700, 
								1117642500, 1117644300, 1117646100, 1117647900, 1117649700, 1117651500, 
								1117653300, 1117655100, 1117656900, 1117658700, 1117660500, 1117662300, 
								1117664100, 1117665900, 1117667700, 1117669500, 1117671300, 1117673100, 
								1117674900, 1117676700, 1117678500, 1117680300, 1117682100, 1117683900, 
								1117685700, 1117687500, 1117689300, 1117691100, 1117692900, 1117694700, 
								1117696500, 1117698300, 1117700100, 1117701900, 1117703700, 1117705500, 
								1117707300, 1117709100, 1117710900, 1117712700, 1117714500, 1117716300, 
								1117718100, 1117719900, 1117721700, 1117723500, 1117725300, 1117727100, 
								1117728900, 1117730700, 1117732500, 1117734300, 1117736100, 1117737900, 
								1117739700, 1117741500, 1117743300, 1117745100, 1117746900, 1117748700, 
								1117750500, 1117752300, 1117754100, 1117755900, 1117757700, 1117759500, 
								1117761300, 1117763100, 1117764900, 1117766700, 1117768500, 1117770300, 
								1117772100, 1117773900, 1117775700, 1117777500, 1117779300, 1117781100, 
								1117782900, 1117784700, 1117786500, 1117788300, 1117790100, 1117791900, 
								1117793700, 1117795500, 1117797300, 1117799100, 1117800900, 1117802700, 
								1117804500, 1117806300, 1117808100, 1117809900, 1117811700, 1117813500, 
								1117815300, 1117817100, 1117818900, 1117820700, 1117822500, 1117824300, 
								1117826100, 1117827900, 1117829700, 1117831500, 1117833300, 1117835100, 
								1117836900, 1117838700, 1117840500, 1117842300, 1117844100, 1117845900, 
								1117847700, 1117849500, 1117851300, 1117853100, 1117854900, 1117856700, 
								1117858500, 1117860300, 1117862100, 1117863900, 1117865700, 1117867500, 
								1117869300, 1117871100, 1117872900, 1117874700, 1117876500, 1117878300, 
								1117880100, 1117881900, 1117883700, 1117885500, 1117887300, 1117889100, 
								1117890900, 1117892700, 1117894500, 1117896300, 1117898100, 1117899900, 
								1117901700, 1117903500, 1117905300, 1117907100, 1117908900, 1117910700, 
								1117912500, 1117914300, 1117916100, 1117917900, 1117919700, 1117921500, 
								1117923300, 1117925100, 1117926900, 1117928700, 1117930500, 1117932300, 
								1117934100, 1117935900, 1117937700, 1117939500, 1117941300, 1117943100, 
								1117944900, 1117946700, 1117948500, 1117950300, 1117952100, 1117953900, 
								1117955700, 1117957500, 1117959300, 1117961100, 1117962900, 1117964700, 
								1117966500, 1117968300, 1117970100, 1117971900, 1117973700, 1117975500, 
								1117977300, 1117979100, 1117980900, 1117982700, 1117984500, 1117986300, 
								1117988100, 1117989900, 1117991700, 1117993500, 1117995300, 1117997100, 
								1117998900, 1118000700, 1118002500, 1118004300, 1118006100, 1118007900, 
								1118009700, 1118011500, 1118013300, 1118015100, 1118016900, 1118018700, 
								1118020500, 1118022300, 1118024100, 1118025900, 1118027700, 1118029500, 
								1118031300, 1118033100, 1118034900, 1118036700, 1118038500, 1118040300, 
								1118042100, 1118043900, 1118045700, 1118047500, 1118049300, 1118051100, 
								1118052900, 1118054700, 1118056500, 1118058300, 1118060100, 1118061900, 
								1118063700, 1118065500, 1118067300, 1118069100, 1118070900, 1118072700, 
								1118074500, 1118076300, 1118078100, 1118079900, 1118081700, 1118083500, 
								1118085300, 1118087100, 1118088900, 1118090700, 1118092500, 1118094300, 
								1118096100, 1118097900, 1118099700, 1118101500, 1118103300, 1118105100, 
								1118106900, 1118108700, 1118110500, 1118112300, 1118114100, 1118115900, 
								1118117700, 1118119500, 1118121300, 1118123100, 1118124900, 1118126700, 
								1118128500, 1118130300, 1118132100, 1118133900, 1118135700, 1118137500, 
								1118139300, 1118141100, 1118142900, 1118144700, 1118146500, 1118148300, 
								1118150100, 1118151900, 1118153700, 1118155500, 1118157300, 1118159100, 
								1118160900, 1118162700, 1118164500, 1118166300, 1118168100, 1118169900, 
								1118171700, 1118173500, 1118175300, 1118177100, 1118178900, 1118180700, 
								1118182500, 1118184300, 1118186100, 1118187900), class = c("POSIXct", 
								"POSIXt"), tzone = "GMT"), FP_VARnight = c(NA, NA, NA, NA, 10.481, 
						6.9146, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, 12.297, 4.9481, 9.6572, 4.3965, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 3.7993, 2.6716, NA, 
						NA, 3.1401, 4.566, 8.4319, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 5.4763, 
						1.3499, 7.1467, 7.0962, 7.1163, -0.3578, -2.6099, 1.3375, 4.4838, 
						9.2856, -1.3787, 1.202, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 13.814, 12.937, 9.569, 
						14.31, NA, NA, NA, NA, 15.872, 9.037, 8.9365, 9.0889, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, 8.585, 3.8082, 4.1164, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, 2.9048, 7.5273, 5.2465, 7.6938, 5.6108, 3.852, 
						5.2484, 3.1916, 4.8825, 5.9027, 5.4587, 4.5179, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						4.9599, 5.1584, 5.3793, 5.1002, 4.8946, 5.1538), FP_VARday = c(NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, -2.1773, -6.3264, -11.035, 
						-8.6794, -15.346, -17.119, -14.159, NA, NA, -26.423, -23.179, 
						-17.59, -32.764, -26.595, -22.906, -16.389, -17.918, -13.701, 
						-11.385, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						3.3124, 0.82198, -6.4674, -12.133, -10.257, -16.264, -19.573, 
						-13.055, -12.478, -12.945, -17.63, -12.616, -17.627, -14.098, 
						-22.728, -22.436, -23.151, -25.771, -20.014, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, -14.268, -16.778, 
						-20.681, -17.547, -8.0527, -11.559, -20.688, -16.436, -19.922, 
						-10.691, -11.51, -16.63, -21.414, -18.158, -16.517, -22.593, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, -3.4536, 
						-8.2624, -12.238, -13.796, -18.776, -21.32, -19.502, -14.894, 
						-19.564, -21.574, -24.184, -26.537, -21.996, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, -1.7246, 
						-0.75699, -5.9802, -12.74, -13.387, -18.471, -18.161, -18.679, 
						-22.757, -23.33, -20.977, -17.059, -16.821, -16.363, -20.323, 
						-25.011, -16.13, -23.201, -22.014, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, 4.7906, -6.692, -6.7622, -12.669, -21.045, 
						-18.8, -28.755, -24.385, -20.186, -21.991, -20.926, -20.001, 
						-12.401, NA, NA, NA, NA, -12.242, -11.325, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA, NA, NA, NA, NA, NA, NA, -0.044762, -5.7203, -12.205, 
						-6.0035, -11.77, -12.764, -8.1143, -13.73, -15.492, -16.532, 
						-20.261, -15.706, -13.081, -16.018, NA, NA, NA, -19.407, -21.303, 
						NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
						NA, NA), NEW_FP_Temp = c(9.726, 9.5525, 8.6044, 8.3625, 8.2068, 
						7.9927, 7.6855, 7.6087, 7.1951, 7.1703, 7.6197, 7.0283, 7.0489, 
						7.5313, 8.099, 8.8675, 8.694, 9.1625, 10.585, 11.261, 11.546, 
						10.822, 11.079, 11.865, 11.362, 12.613, 12.584, 12.579, 12.311, 
						11.869, 11.084, 10.978, 10.827, 11.124, 11.034, 10.565, 10.323, 
						10.494, 11.069, 10.377, 9.9492, 9.5636, 9.5419, 9.4732, 9.3611, 
						9.2415, 9.154, 9.0479, 8.5811, 8.431, 8.5787, 8.5314, 8.4269, 
						8.5821, 8.5268, 8.4881, 8.4899, 8.3313, 7.5446, 6.1565, 6.5678, 
						7.8799, 8.2569, 8.7638, 9.1221, 9.4673, 10.98, 11.181, 10.799, 
						11.173, 11.096, 12.065, 11.616, 12.112, 11.485, 12.375, 12.344, 
						11.994, 12.534, 12.057, 12.276, 13.215, 13.725, 13.581, 13.214, 
						13.031, 12.958, 12.19, 10.853, 9.5252, 9.1383, 8.904, 8.9118, 
						9.2762, 9.1156, 9.0046, 8.7291, 8.3949, 8.2723, 7.9256, 7.1679, 
						7.2506, 6.9353, 6.5604, 6.4071, 6.2387, 6.0244, 6.2359, 6.6059, 
						8.1083, 9.932, 12.484, 12.113, 12.112, 12.585, 12.531, 11.654, 
						11.995, 12.573, 13.635, 13.38, 13.595, 14.179, 14.506, 15.266, 
						15.593, 15.747, 15.825, 15.669, 16.066, 16.163, 15.925, 15.219, 
						13.811, 13.351, 13.588, 12.774, 12.138, 11.081, 10.809, 10.489, 
						10.455, 10.639, 10.658, 10.809, 9.5976, 8.3, 8.362, 8.7748, 8.1126, 
						7.8459, 8.3323, 8.28, 8.0634, 8.3038, 8.361, 10.07, 10.83, 11.207, 
						11.749, 12.454, 13.14, 13.848, 13.78, 13.33, 13.27, 13.502, 13.394, 
						14.455, 14.619, 15.128, 14.918, 15.254, 14.783, 15.421, 15.348, 
						15.21, 14.976, 14.713, 14.167, 13.556, 13.832, 14.491, 13.29, 
						12.005, 11.418, 11.204, 10.646, 9.6177, 9.6859, 9.8205, 9.487, 
						9.3662, 10.06, 10.191, 10.259, 9.914, 9.487, 10.101, 8.9946, 
						8.649, 8.4125, 8.2257, 8.0333, 8.2477, 8.779, 9.3006, 9.8425, 
						10.621, 11.832, 12.767, 12.653, 13.413, 13.939, 14.042, 13.736, 
						13.33, 13.292, 13.232, 13.487, 13.674, 12.981, 13.138, 13.797, 
						13.13, 12.287, 12.635, 11.996, 11.505, 11.294, 11.041, 10.624, 
						10.12, 9.8648, 9.6907, 9.4476, 9.2683, 8.9167, 8.7187, 8.537, 
						8.613, 8.4771, 8.3181, 7.5376, 7.1788, 6.913, 6.5723, 6.2942, 
						6.0041, 5.9523, 6.1177, 6.1897, 6.5375, 6.7323, 6.649, 6.7356, 
						7.0747, 7.479, 8.2957, 9.4231, 9.9518, 9.5785, 10.399, 10.048, 
						10.503, 10.405, 10.185, 9.04167, 9.1355, 10.052, 9.1041, 9.6987, 
						9.9247, 10.584, 11.133, 11.1, 11.694, 10.123, 9.2262, 9.1733, 
						9.1696, 8.2873, 8.1501, 7.9445, 7.825, 7.662, 7.5052, 7.6092, 
						7.412, 7.0818, 6.7081, 6.5346, 7.1461, 7.2744, 7.399, 7.4812, 
						7.4005, 7.0471, 5.9185, 5.4261, 5.6355, 6.1223, 7.1273, 8.2773, 
						7.5538, 8.0126, 8.9936, 9.2634, 9.4761, 10.115, 10.958, 11.54, 
						11.205, 11.203, 10.652, 10.607, 10.405, 11.619, 12.939, 13.1, 
						12.576, 12.842, 12.626, 12.673, 12.214, 11.614, 10.862, 9.598, 
						7.8465, 6.8725, 5.6394, 4.7237, 4.0641, 3.5038, 3.4563, 3.2876
				), NEW_FP_VPD = c(0.012059, 0.0096554, 0.013529, 0.026837, 0.60009, 
						1.3009, 1.3026, 1.6123, 1.3286, 1.7681, 2.7739, 2.4346, 2.8528, 
						3.09, 3.5692, 3.9768, 3.9102, 4.0255, 5.2644, 5.0494, 3.8873, 
						1.8708, 1.9068, 2.6337, 1.828, 3.7669, 2.6925, 2.3656, 1.3954, 
						0.9486, 0.50772, 0.91613, 1.4934, 2.0622, 2.1364, 2.0304, 1.8855, 
						1.9915, 2.4076, 2.0846, 1.666, 1.3656, 1.6043, 1.2712, 1.3628, 
						1.6278, 1.4511, 1.3247, 0.78801, 0.3041, 0.12222, 0.17423, 0.13654, 
						1.1272, 1.3282, 1.5482, 1.7077, 1.2598, 0.8888, 0.2676, 0.32297, 
						1.0866, 1.2624, 1.6628, 1.7227, 1.719, 2.5392, 2.8242, 2.8225, 
						2.2816, 2.2538, 2.8606, 2.5795, 2.3411, 1.9596, 2.7333, 2.6078, 
						2.6911, 3.2058, 2.9536, 3.3036, 3.7248, 4.3226, 4.2024, 4.0195, 
						4.0355, 4.1884, 3.6269, 2.5308, 1.7034, 1.5916, 1.5459, 1.5777, 
						1.7806, 1.6948, 1.5979, 1.3784, 1.0659, 0.86797, 0.56021, 0.24037, 
						0.20707, 0.13509, 0.094801, 0.089186, 0.076078, 0.070835, 0.07863, 
						0.086223, 0.4255, 1.1186, 2.9795, 2.4781, 2.3806, 2.6233, 2.7956, 
						2.2529, 2.0826, 2.1394, 2.8758, 2.8235, 2.8269, 3.3625, 3.2353, 
						3.8566, 4.2046, 5.1439, 4.7686, 4.312, 4.5772, 5.0241, 5.0928, 
						4.9221, 4.2877, 3.9888, 4.2336, 3.75, 3.3877, 2.6396, 2.2414, 
						1.9126, 1.7684, 1.9692, 1.9991, 2.0528, 1.3388, 0.57565, 0.60832, 
						1.0252, 0.55452, 0.513, 0.92717, 0.83047, 0.62487, 0.79063, 0.93568, 
						2.0694, 2.3135, 2.5952, 2.8026, 3.0786, 3.9049, 4.48, 4.8034, 
						4.394, 4.4072, 4.5803, 4.4532, 5.0084, 4.9204, 5.315, 5.1103, 
						5.3057, 5.0342, 5.6663, 5.7366, 5.7005, 5.2662, 5.1065, 4.5596, 
						3.798, 4.1452, 4.3277, 3.1365, 2.2969, 2.2102, 2.4242, 1.667, 
						1.2019, 1.2223, 1.2828, 0.94761, 0.64307, 0.92743, 0.76639, 0.87025, 
						0.77942, 0.34226, 0.52522, 0.06578, 0.043625, 0.045138, 0.091862, 
						0.14143, 0.5508, 1.3078, 1.526, 1.911, 3.0242, 4.6053, 5.4413, 
						5.7801, 6.3188, 6.0157, 5.4618, 5.6148, 4.2011, 3.9518, 3.7923, 
						3.0436, 2.473, 2.0286, 1.7473, 3.1916, 2.2914, 2.2379, 2.7413, 
						1.8481, 1.1779, 0.91065, 0.72563, 0.29877, 0.092992, 0.082165, 
						0.068938, 0.060601, 0.044905, 0.035972, 0.023324, 0.012577, 0.014991, 
						0.0048773, 0.0064694, 0.012582, 0.01187, 0.0098621, 0.017614, 
						0.017565, 0.028912, 0.043073, 0.03942, 0.04464, 0.070091, 0.074678, 
						0.052533, 0.034248, 0.04825, 0.058201, 0.15657, 1.0961, 1.3636, 
						1.5353, 2.1206, 1.7771, 2.6961, 2.9803, 1.7307, 0.36775, 0.33262, 
						1.2731, 0.21777, 0.45406, 1.0828, 1.2009, 2.0479, 1.9906, 2.6635, 
						1.1259, 0.78349, 0.56388, 0.8459, 0.16349, 0.32903, 0.65062, 
						0.67196, 0.87929, 0.51251, 0.3446, 0.29586, 0.26606, 0.15746, 
						0.099486, 0.14971, 0.13562, 0.10567, 0.09757, 0.1028, 0.1477, 
						0.10008, 0.082787, 0.086919, 0.08094, 0.072789, 0.18973, 0.048299, 
						0.12276, 0.78265, 1.2179, 1.9818, 2.6076, 3.1664, 3.5429, 4.4342, 
						4.7321, 4.1458, 4.8489, 3.7802, 5.1675, 6.5057, 7.485, 7.6253, 
						8.9834, 9.5679, 9.7326, 9.4855, 9.1803, 8.4881, 7.3363, 6.0861, 
						5.4925, 4.5063, 4.02, 3.7849, 3.7167, 3.9498, 3.9885), Rg = c(0, 
						0, 0, 0, 0, 0, 0, 0, 0, 0, 0.91732, 11.893, 51.97, 113.61, 250.62, 
						214.07001, 226.53, 346.85001, 376.57999, 619.35999, 697.33002, 
						835.06, 914.02002, 746.97998, 429.32999, 979.27002, 692.10999, 
						811.47998, 652.70001, 596.16998, 352.64001, 285.25, 251.41, 363.35999, 
						272, 187.11, 140.48, 167.95, 225.95, 115.07, 43.234, 11.735, 
						0.23214, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.2667, 
						11.596, 18.425, 28.792, 62.741, 137.73, 232.23, 310.89001, 574.40997, 
						562.44, 287.73999, 374.25, 388.09, 380.64999, 276.34, 526.62, 
						301.26999, 806.38, 745.69, 591.69, 662.19, 471.67001, 453.28, 
						618.09003, 629.47998, 500.06, 379.64999, 297.32999, 201.81, 102.53, 
						35.065, 10.115, 0.89625, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
						0, 0, 0, 0.72895, 8.6647, 43.093, 130.12, 207.19, 298.92999, 
						445.23999, 458.09, 524.47998, 386.10999, 189.7, 261.51001, 526.51001, 
						694.56, 373.01999, 373.56, 429.98001, 434.92001, 935.51001, 687.70001, 
						606.76001, 787.58002, 715.33002, 671.40002, 552.46997, 440.28, 
						263.17001, 92.886, 96.049, 76.38, 30.957, 6.7027, 0.96976, -0.03677, 
						-0.01379, 0, -0.0046, 0, -0.0023, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
						0.99562, 16.622, 32.359, 57.964, 92.202, 162.67, 222.48, 458.16, 
						449.67999, 376.23001, 255.71001, 300.29999, 424.76001, 346.79999, 
						844.51001, 777.38, 697.69, 724.71002, 594.20001, 549.78003, 675.60999, 
						609.65997, 726.42999, 495.98999, 513.08002, 286.26999, 245.50999, 
						308.37, 286.23001, 108.47, 36.379, 16.54, 1.4754, -0.0023, 0, 
						-0.0023, 0, 0, 0, 0, 0, -0.0023, -0.0023, 0, 0, 0, 0, 0, 0.80468, 
						7.5527, 22.415, 38.43, 62.573, 122, 303.41, 399.82001, 535.79999, 
						533.35999, 615.78998, 774.57001, 817.59003, 775.35999, 455.35999, 
						408.75, 338.91, 532.17999, 638.07001, 366.70001, 464.34, 575.45001, 
						463.41, 233.41, 282.75, 242.03999, 191.38, 233.69, 57.333, 70.334, 
						31.216, 9.1632, 0.33557, -0.0023, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
						0, 0, 0, 0, 0, 0.46449, 4.9898, 14.153, 60.93, 121.16, 149.21001, 
						224.49001, 349.60001, 452.28, 656.47998, 765.69, 368.92001, 741.34998, 
						378.23999, 465.54001, 262.17001, 260.39001, 228.65414, 369.92001, 
						506.73001, 269.76001, 465.16, 374.44, 552.92999, 518.96002, 586.16998, 
						503.35001, 180.85001, 69.481, 55.818, 53.621, 14.224, 1.3423, 
						0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.1566, 13.153, 
						40.591, 66.41, 173.56, 188, 116.5, 318.75, 306.32001, 224.25, 
						252.12, 281.70001, 634.09998, 519.03003, 285.32001, 375.14001, 
						255.89999, 297.32001, 318.57001, 677.41998, 946.20001, 891.38, 
						635.35999, 693.84003, 595.92999, 505.04999, 413.34, 329.59, 245.88, 
						166.75999, 24.339, 10.433, 2.2341, 0, 0, 0, 0, 0)), .Names = c("sDateTime", 
				"FP_VARnight", "FP_VARday", "NEW_FP_Temp", "NEW_FP_VPD", "Rg"
		), row.names = 7249:7584, class = "data.frame")
dsNEE$NEE_fsd <- 0.05*dsNEE$FP_VARday		


#from test_that("partGLFitLRCWindows outputs are in accepted range"
resLRCEx1 <- structure(list(resOptList = list(structure(list(opt.parms.V = structure(c(0, 
														28.1552114607447, 0.264181937859435, 5.16758715278128, 185.617678757497
												), .Names = c("k", "beta0", "alfa", "Rb", "E0")), iOpt = 1:5, 
										initialGuess.parms.V.n = structure(c(0, 24.6042824, 0.1, 
														8.68483981783771, 185.617678757497), .Names = c("k", "beta0", 
														"alfa", "Rb", "E0")), covParms = structure(c(0, 0, 0, 0, 
														0, 0, 0.0164521595788231, 0.000150948115388241, 0.00470082516700928, 
														0, 0, 0.000150948115388242, 4.46655799736124e-05, 0.000745966808293157, 
														0, 0, 0.00470082516700926, 0.000745966808293157, 0.0135164099818451, 
														0, 0, 0, 0, 0, 20816.0942091486), .Dim = c(5L, 5L), .Dimnames = list(
														c("k", "beta0", "alfa", "Rb", "E0"), c("k", "beta0", 
																"alfa", "Rb", "E0")))), .Names = c("opt.parms.V", "iOpt", 
										"initialGuess.parms.V.n", "covParms")), structure(list(opt.parms.V = structure(c(0, 
														25.964317012826, 0.171170666057465, 2.1061767282917, 185.617678757497
												), .Names = c("k", "beta0", "alfa", "Rb", "E0")), iOpt = 1:5, 
										initialGuess.parms.V.n = structure(c(0, 23.6355054, 0.1, 
														8.68483981783771, 185.617678757497), .Names = c("k", "beta0", 
														"alfa", "Rb", "E0")), covParms = structure(c(0, 0, 0, 0, 
														0, 0, 0.0249811161375782, -0.000150194670672609, -0.000908018269739324, 
														0, 0, -0.00015019467067261, 1.28730683402289e-05, 0.000229359136707699, 
														0, 0, -0.000908018269739337, 0.000229359136707699, 0.00480458219392517, 
														0, 0, 0, 0, 0, 20816.0942091486), .Dim = c(5L, 5L), .Dimnames = list(
														c("k", "beta0", "alfa", "Rb", "E0"), c("k", "beta0", 
																"alfa", "Rb", "E0")))), .Names = c("opt.parms.V", "iOpt", 
										"initialGuess.parms.V.n", "covParms")), structure(list(opt.parms.V = structure(c(0, 
														28.1975014240472, 0.147048891270042, 2.3663375897427, 185.617678757497
												), .Names = c("k", "beta0", "alfa", "Rb", "E0")), iOpt = 1:5, 
										initialGuess.parms.V.n = structure(c(0, 24.33727084, 0.1, 
														8.68483981783771, 185.617678757497), .Names = c("k", "beta0", 
														"alfa", "Rb", "E0")), covParms = structure(c(0, 0, 0, 0, 
														0, 0, 0.0538604094839747, -0.000431982958065247, -0.0063804899129617, 
														0, 0, -0.00043198295806524, 8.38000915150449e-06, 0.000127379438091679, 
														0, 0, -0.00638048991296168, 0.000127379438091679, 0.0019417927403693, 
														0, 0, 0, 0, 0, 20816.0942091486), .Dim = c(5L, 5L), .Dimnames = list(
														c("k", "beta0", "alfa", "Rb", "E0"), c("k", "beta0", 
																"alfa", "Rb", "E0")))), .Names = c("opt.parms.V", "iOpt", 
										"initialGuess.parms.V.n", "covParms")), structure(list(opt.parms.V = structure(c(0, 
														28.0941328298213, 0.1, 1.6281561256414, 186.3567923302), .Names = c("k", 
														"beta0", "alfa", "Rb", "E0")), iOpt = c(1L, 2L, 4L, 5L), initialGuess.parms.V.n = structure(c(0, 
														18.2353459, 0.1, 8.61826241156938, 186.3567923302), .Names = c("k", 
														"beta0", "alfa", "Rb", "E0")), covParms = structure(c(0, 0, 0, 
														0, 0, 0, 0.147906649803781, 0, 0.000411411419094437, 0, 0, 0, 
														0, 0, 0, 0, 0.000411411419094437, 0, 5.66748908042254e-06, 0, 
														0, 0, 0, 0, 22547.7065470622), .Dim = c(5L, 5L), .Dimnames = list(
														c("k", "beta0", "alfa", "Rb", "E0"), c("k", "beta0", "alfa", 
																"Rb", "E0")))), .Names = c("opt.parms.V", "iOpt", "initialGuess.parms.V.n", 
										"covParms"))), summary = structure(list(Start = c(1, 3, 5, 7), 
								End = c(4, 6, 8, 10), Num = c(65L, 63L, 50L, 16L), iMeanRec = c(87L, 
										190L, 257L, 308L), iCentralRec = c(96, 192, 288, 312), iFirstRec = c(1L, 
										97L, 193L, 289L), E_0 = c(185.617678757497, 185.617678757497, 
										185.617678757497, 186.3567923302), E_0_SD = c(144.277836860512, 
										144.277836860512, 144.277836860512, 150.158937619651), R_ref12 = c(8.68483981783771, 
										8.68483981783771, 8.68483981783771, 8.61826241156938), R_ref = c(5.16758715278128, 
										2.1061767282917, 2.3663375897427, 1.6281561256414), R_ref_SD = c(0.11626009625768, 
										0.0693150935505765, 0.0440657774284001, 0.00238064887802098
								), a = c(0.264181937859435, 0.171170666057465, 0.147048891270042, 
										0.1), a_SD = c(0.00668323125244162, 0.00358790584327807, 
										0.00289482454589298, NA), b = c(28.1552114607447, 25.964317012826, 
										28.1975014240472, 28.0941328298213), b_SD = c(0.128265972022291, 
										0.158054155711193, 0.232078455449821, 0.384586335955636), 
								k = c(0, 0, 0, 0), k_SD = c(0, 0, 0, 0), parms_out_range = c(1L, 
										1L, 1L, 1L)), .Names = c("Start", "End", "Num", "iMeanRec", 
								"iCentralRec", "iFirstRec", "E_0", "E_0_SD", "R_ref12", "R_ref", 
								"R_ref_SD", "a", "a_SD", "b", "b_SD", "k", "k_SD", "parms_out_range"
						), row.names = c(NA, 4L), class = "data.frame")), .Names = c("resOptList", 
				"summary"))


test_that("partGL_RHLightResponseGrad matches numerical estimates",{
			ds <- with(dsNEE, data.frame(NEE=FP_VARnight, Temp=NEW_FP_Temp, VPD=NEW_FP_VPD, Rg=ifelse( Rg >= 0, Rg, 0 )))
			ds$NEE[!is.na(dsNEE$FP_VARday)] <- dsNEE$FP_VARday[!is.na(dsNEE$FP_VARday)]
			ds$sdNEE <- 0.05*ds$NEE
			#str(ds)
			theta0 <- structure(c(0, 27.3333395589509, 0.162207578338878, 2.59392002410639, 185
					), .Names = c("k", "beta0", "alfa", "Rb","E0"))
			res <- partGL_RHLightResponseGrad(theta0, Rg=ds$Rg, VPD=ds$VPD, Temp=ds$Temp)
			.numDerivLRC <- function(theta, eps=0.0001, ...){
				ans <- matrix( NA, nrow=length(list(...)[[1]]), ncol=length(theta), dimnames=list(NULL,names(theta)))
				i <- 1L
				for( i in seq_along(theta)){
							thetaMinus <- theta; thetaMinus[i] <- theta[i]-eps
							thetaPlus <- theta; thetaPlus[i] <- theta[i]+eps
							fMinus <- partGL_RHLightResponse(thetaMinus, ...)$NEP
							fPlus <- partGL_RHLightResponse(thetaPlus, ...)$NEP
							ans[,i] <- derivI <- (fPlus - fMinus)/(2*eps)
						}
				ans
			}
			res2 <- .numDerivLRC(theta=theta0, Rg=ds$Rg, VPD=ds$VPD, Temp=ds$Temp)
			expect_true( all(abs(res$NEP - res2) < 1e-2))
			#plot( res$NEP[,4L] ~ res2[,4L])
		})

test_that("estimating temperature sensitivity outputs are in accepted range",{
			dss <- subset(dsNEE, as.POSIXlt(dsNEE$sDateTime)$mday %in% 1:8 
							& !is.na(dsNEE$FP_VARnight))
			dss <- dss[ order(dss$NEW_FP_Temp), ]
			res <- partGLEstimateTempSensInBounds(dss$FP_VARnight, dss$NEW_FP_Temp+273.15)
			expect_true( res$E_0 >= 50 && res$E_0 < 400 )
			.tmp.plot <- function(){
				plot( FP_VARnight ~ NEW_FP_Temp, dss)		# FP_VARnight negative?
				p <- coef(res$resFit)
				lines( fLloydTaylor(p[1], p[2], dss$NEW_FP_Temp+273.15) ~ dss$NEW_FP_Temp)#
			}
		})

test_that("RHLightResponseCostC",{
			.tmp.reloadDll <- function(){
				library.dynam.unload("REddyProc", file.path(.libPaths()[1],"REddyProc") )
				installPkg()
				library.dynam("REddyProc","REddyProc", .libPaths()[1] )
			}
			#
			dss <- subset(dsNEE, as.POSIXlt(dsNEE$sDateTime)$mday %in% 1:8 )
			dssDay <- subset(dss, !is.na(dsNEE$FP_VARday) )
			theta <- c(k=0, beta0=28.6, alfa=0.18,  Rb=2.87, E0=185)
			flux <- dssDay$FP_VARday
			sdFlux <- 0.05*dssDay$FP_VARday
			betaPrior <- 26
			sdBetaPrior <- 0.3*betaPrior / sqrt(length(flux))
			parameterPrior <- c(0,betaPrior,8,15, 185)
			sdParameterPrior <- c(NA,sdBetaPrior,NA,NA, NA)
			predR <- partGL_RHLightResponse(theta
				,dssDay$Rg, dssDay$NEW_FP_VPD, dssDay$NEW_FP_Temp, 10.0, FALSE)
			RSSR <- .partGLRHLightResponseCost( theta, theta, seq_along(theta), flux, sdFlux, parameterPrior, sdParameterPrior
					,dssDay$Rg, dssDay$NEW_FP_VPD, dssDay$NEW_FP_Temp, useCVersion=FALSE)
			tmp <- RHLightResponseCostC( theta, flux, sdFlux, parameterPrior, sdParameterPrior
					,dssDay$Rg, dssDay$NEW_FP_VPD, dssDay$NEW_FP_Temp, 10.0, FALSE)
			#(predR$NEP - tmp)					
			expect_true(tmp - RSSR < 1e-8)	
		})

.benchmark_RHLightResponseCostC <- function(){
	#require(rbenchmark)
	tmp <- benchmark( 
	 .partGLRHLightResponseCost( theta[1:4], theta, 1:4, flux, sdFlux, parameterPrior, sdParameterPrior
			 ,dssDay$Rg, dssDay$NEW_FP_VPD, dssDay$NEW_FP_Temp, useCVersion=FALSE)
		,
		RHLightResponseCostC( theta, flux, sdFlux, parameterPrior, sdParameterPrior
				,dssDay$Rg, dssDay$NEW_FP_VPD, dssDay$NEW_FP_Temp, 10.0, FALSE)
		,replications = 10000
					)
	tmp			#speedup of only 2 :(

	tmp <- benchmark( 
			.partGLRHLightResponseCost( theta[1:4], theta, 1:4, flux, sdFlux, parameterPrior, sdParameterPrior
					,dssDay$Rg, dssDay$NEW_FP_VPD, dssDay$NEW_FP_Temp, useCVersion=FALSE)
			,
			.partGLRHLightResponseCost( theta[1:4], theta, 1:4, flux, sdFlux, parameterPrior, sdParameterPrior
					,dssDay$Rg, dssDay$NEW_FP_VPD, dssDay$NEW_FP_Temp)
			,replications = 10000
	)
	tmp			#speedup of only about 1.8 :(
}



test_that("partGLFitLRC",{
			dss <- subset(dsNEE, as.POSIXlt(dsNEE$sDateTime)$mday %in% 1:8 )
			dssDay <- subset(dss, !is.na(dsNEE$FP_VARday) )
			dssNight <- subset(dss, !is.na(dsNEE$FP_VARnight) )
			dsDay <- data.frame( NEE=dssDay$FP_VARday, sdNEE=dssDay$NEE_fsd, Rg=dssDay$Rg, Temp=dssDay$NEW_FP_Temp, VPD=dssDay$NEW_FP_VPD)
			res <- resNewPrior <- partGLFitLRC(dsDay, dssNight$FP_VARnight, E_0.n=185, sdE_0.n=.05*185,R_refNight.n=mean(dssNight$FP_VARnight, na.rm=TRUE), lastGoodParameters.V.n=NA_real_
					,controlGLPart.l=partGLControl(nBootUncertainty=10L)
			)
			#testing Lasslop compliency: different priors, covariance from fit
			res <- resGLPrior <- partGLFitLRC(dsDay, dssNight$FP_VARnight, E_0.n=185, sdE_0.n=.05*185, R_refNight.n=mean(dssNight$FP_VARnight, na.rm=TRUE),lastGoodParameters.V.n=NA_real_
					,controlGLPart.l=partGLControl(nBootUncertainty=0L, isLasslopPriorsApplied=TRUE)
			)
			#dput(res$opt.parms.V)
			.tmp.plot <- function(){
				dsDay <- dsDay[ order(dsDay$Rg), ]
				plot( -NEE ~ Rg, dsDay)		# FP_VARnight negative?
				p <- res$opt.parms.V
				pred <- partGL_RHLightResponse(p, Rg=dsDay$Rg, VPD=dsDay$VPD, Temp=dsDay$Temp, E0=185)
				lines(pred$NEP  ~ dsDay$Rg)
			}
			# testing increasing number of bootstrap samples
			.tmp.f <- function(){
				(res60 <- partGLFitLRC(dsDay, dssNight$FP_VARnight, E_0.n=185, sdE_0.n=.05*185, R_refNight.n=mean(dssNight$FP_VARnight, na.rm=TRUE)
						,controlGLPart.l=partGLControl(nBootUncertainty=100L)
				))
			}		
		})



		
test_that("partGLFitLRCWindows outputs are in accepted range",{
			ds <- with(dsNEE, data.frame(NEE=FP_VARnight, Temp=NEW_FP_Temp, VPD=NEW_FP_VPD, Rg=ifelse( Rg >= 0, Rg, 0 )))
			ds$NEE[!is.na(dsNEE$FP_VARday)] <- dsNEE$FP_VARday[!is.na(dsNEE$FP_VARday)]
			ds$sdNEE <- 0.05*ds$NEE
			ds$isDay <- is.finite(dsNEE$FP_VARday)
			ds$isNight <- is.finite(dsNEE$FP_VARnight)
			#yday <- as.POSIXlt(dsNEE$sDateTime)$yday	
			resFits <- partGLFitLRCWindows(ds, nRecInDay=48L, controlGLPart.l=partGLControl(nBootUncertainty=10L))
			expect_equal( nrow(resFits$summary), length(resFits$resOptList) )
			.tmp.f <- function(){
				# in order to replicate, use nBoot=0L 
				resParms0 <- partGLFitLRCWindows(ds, nRecInDay=48L, controlGLPart.l=partGLControl(nBootUncertainty=0L))
				dput(resParms0)
			}
			# check the conditions of Lasslop10 Table A1
			resSummary <- resFits$summary
			expect_true( all(resSummary$E_0 >= 50 & resSummary$E_0 <= 400) )
			expect_true( all(resSummary$R_ref > 0) )
			expect_true( all(resSummary$a >= 0 ) )
			expect_true( all(resSummary$a[-1] < 0.22) )	# first value may be greater, due to previous estimate
			expect_true( all(resSummary$b >= 0 & resSummary$b < 250) )
			expect_true( all(ifelse(resSummary$b > 100, resSummary$b_SD < resSummary$b, TRUE) ))
			expect_true( all(resSummary$k >= 0) )
			expect_true( !all(is.na(resSummary$R_ref_SD)))
			expect_true( all(resSummary$iMeanRec < nrow(ds)) )
			expect_true( all(resSummary$iCentralRec < nrow(ds)) )
			
			.tmp.inspectYear <- function(){
				# dsYear generated from inside sPartitionGL
				dsYear <- local({ load("tmp/dsTestPartitioningLasslop10.RData"); get(ls()[1]) })
				dsYear2 <- with(dsYear, data.frame(NEE=NEE_f, sdNEE=NEE_fsd, Temp=NEW_FP_Temp, VPD=NEW_FP_VPD, Rg=ifelse( Rg >= 0, Rg, 0 )
								,isDay=!is.na(FP_VARday), isNight=!is.na(FP_VARnight) 				
						))
				resY <- resY1 <- partGLFitLRCWindows(dsYear2, nRecInDay=48L, controlGLPart.l=partGLControl(nBootUncertainty=30L)	)
				#resY <- resYL <- partGLFitLRCWindows(dsYear2, nRecInDay=48L, controlGLPart.l=partGLControl(isLasslopPriorsApplied=TRUE, nBootUncertainty=0L)	)
				resSummary <- resY$summary
				#resParms <- resY
				head(resSummary)				
				plot( R_ref ~ Start, resSummary)
				#points( R_ref ~ Start, resSummary, col="red")
				#points( R_ref ~ Start, resSummary, col="blue")
				with(resSummary, segments(Start,R_ref-R_ref_SD,Start, R_ref+R_ref_SD))
				points( R_ref12 ~ Start, resSummary, pch="+")
				plot( b ~ Start, resSummary)
				plot( E_0 ~ Start, resSummary )
				#
				iStarts <- intersect(resYL$summary$Start, resY1$summary$Start)
				sumL <- subset(resYL$summary, Start %in% iStarts)
				sum1 <- subset(resY1$summary, Start %in% iStarts)
				plot(sumL$R_ref ~ sum1$R_ref); abline(0,1)			
				summary(lm( sumL$R_ref  ~ sum1$R_ref-1 ))
				plot(sumL$R_ref_SD ~ sum1$R_ref_SD); abline(0,1)			
				summary(lm( sumL$R_ref_SD  ~ sum1$R_ref_SD-1 ))	# Lasslop sd is smaller
			}
})

test_that(".partGPAssociateSpecialRows correct next lines",{
			expect_error(
					res <- .partGPAssociateSpecialRows(integer(0),9)
			)
			res <- .partGPAssociateSpecialRows(c(3,6,7,9),12)
			expect_true( all(res$wBefore+res$wAfter == 1)) 
			# special rows
			expect_equal( c(iRec=3,iSpecialBefore=1L,iSpecialAfter=1L,iBefore=3, iAfter=3, wBefore=0.5, wAfter=0.5), unlist(res[3,])) 
			expect_equal( c(iRec=6,iSpecialBefore=2L,iSpecialAfter=2L,iBefore=6, iAfter=6, wBefore=0.5, wAfter=0.5), unlist(res[6,]))
			# first rows and last rows
			expect_equal( c(iRec=1,iSpecialBefore=1L,iSpecialAfter=1L,iBefore=3, iAfter=3, wBefore=0.5, wAfter=0.5), unlist(res[1,])) 
			expect_equal( c(iRec=12,iSpecialBefore=4L,iSpecialAfter=4L,iBefore=9, iAfter=9, wBefore=0.5, wAfter=0.5), unlist(res[12,])) 
			# weights after and before special row 6
			expect_equal( rep(3,2), unlist(res[4:5,"iBefore"])) 
			expect_equal( rep(6,2), unlist(res[4:5,"iAfter"])) 
			expect_equal( rep(1,2), unlist(res[4:5,"iSpecialBefore"])) 
			expect_equal( rep(2,2), unlist(res[4:5,"iSpecialAfter"])) 
			expect_equal( (2:1)/3, unlist(res[4:5,"wBefore"])) 
			expect_equal( (1:2)/3, unlist(res[4:5,"wAfter"])) 
			# test last row is special
			res <- .partGPAssociateSpecialRows(c(3,6,7,9),9)
		})
			
			
test_that("partGLInterpolateFluxes runs",{
			tmp <- partGLInterpolateFluxes( dsNEE$Rg, dsNEE$NEW_FP_VPD, dsNEE$NEW_FP_Temp, resLRCEx1)
			expect_equal( nrow(dsNEE), nrow(tmp) )
			.tmp.plot <- function(){
				tmp$time <- dsNEE$sDateTime
				plot( Reco_DT ~ time, tmp)
				plot( GPP_DT ~ time, tmp)
			}
			expect_true( all(c("GPP","Reco") %in% names(tmp) ))
			tmp <- partGLInterpolateFluxes( dsNEE$Rg, dsNEE$NEW_FP_VPD, dsNEE$NEW_FP_Temp, resLRCEx1
							,controlGLPart.l=partGLControl(isSdPredComputed=TRUE))
			expect_equal( nrow(dsNEE), nrow(tmp) )
			expect_true( all(c("GPP","Reco") %in% names(tmp) ))
			expect_true( all(c("sdGPP","sdReco") %in% names(tmp) ))
		})

test_that("partGLPartitionFluxes",{
			dsNEE1 <- dsNEE
			dsNEE1$NEE_f <- dsNEE1$FP_VARnight
			dsNEE1$NEE_f[!is.na(dsNEE1$FP_VARday)] <- dsNEE1$FP_VARday[!is.na(dsNEE1$FP_VARday)]
			dsNEE1$NEE_fqc <- ifelse( is.finite(dsNEE1$NEE_f),0L,1L )
			dsNEE1$Tair_f <- dsNEE1$NEW_FP_Temp
			dsNEE1$Tair_fqc <- ifelse( is.finite(dsNEE1$Tair_f),0L,1L )
			dsNEE1$VPD_f <- dsNEE1$NEW_FP_VPD
			dsNEE1$VPD_fqc <- ifelse( is.finite(dsNEE1$VPD_f),0L,1L )
			dsNEE1$Rg <- ifelse( dsNEE1$Rg >= 0, dsNEE1$Rg, 0 )
			DoY.V.n <- as.POSIXlt(dsNEE1$sDateTime)$yday + 1L
			Hour.V.n <- as.POSIXlt(dsNEE1$sDateTime)$hour + as.POSIXlt(dsNEE1$sDateTime)$min/60
			dsNEE1$PotRad_NEW <- fCalcPotRadiation(DoY.V.n, Hour.V.n, Lat_deg.n=45.0, Long_deg.n=1, TimeZone_h.n=0 )
			tmp <- partitionNEEGL( dsNEE1 )
			expect_equal( nrow(dsNEE1), nrow(tmp) )
			#
			dsNEE2 <- dsNEE1
			names(dsNEE2)[ match(c("NEE_f", "NEE_fqc", "NEE_fsd"),names(dsNEE2))] <- c("NEE_u50_f", "NEE_u50_fqc", "NEE_u50_fsd")
			tmp <- partitionNEEGL( dsNEE2, Suffix.s="u50", controlGLPart.l=partGLControl(nBootUncertainty=0L, isAssociateParmsToMeanOfValids=FALSE) )
			expect_equal( nrow(dsNEE1), nrow(tmp) )
			expect_true( all(is.finite(tmp$GPP_DT_u50)))
			expect_true( all(tmp$GPP_DT_u50 >= 0))
			expect_true( all(tmp$GPP_DT_u50 < 250))
			expect_true( all(tmp$Reco_DT_u50 < 6))
			expect_true( all(tmp$Reco_DT_u50 > 0))
			expect_true( all(tmp$Reco_DT_u50_SD > 0))
			expect_true( all(tmp$GPP_DT_u50_SD >= 0))
			expect_true( all(abs(diff(tmp$Reco_DT_u50)) < 0.6))	#smooth
			# reporting good values at first row
			expect_true( sum( is.finite(tmp$FP_alpha) ) == sum(resLRCEx1$parms_out_range==0L) ) 
			expect_true( all((tmp$FP_alpha[resLRCEx1$iFirstRec] - resLRCEx1$a)[resLRCEx1$parms_out_range==0L] < 1e-2) )
			#expect_true( all((is.na(tmp$FP_alpha[resLRCEx1$iFirstRec] - resLRCEx1$a)[resLRCEx1$parms_out_range!=0L])) )
			.tmp.plot <- function(){
				tmp$time <- dsNEE1$sDateTime
				plot( Reco_DT_u50 ~ time, tmp)
				#plot( diff(Reco_DT_u50) ~ time[-1], tmp)
				plot( GPP_DT_u50 ~ time, tmp)
			}
		})



.profilePartGL <- function(){
	require(profr)
	p1 <- profr({
				for( i in 1:1 ){
					tmp <- partitionNEEGL( dsNEE1 )
				}
			}, 0.01 )
	plot(p1)
	
	
	
}

