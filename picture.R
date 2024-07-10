# Create labelling image from Praatpicture

library(praatpicture)

praatpicture('BeAM_199X_HumanInLandOfDeath_flk_fragment.wav',
             start=0, end=3.3,
             frames=c('sound', 'spectrogram', 'TextGrid'),
             proportion = c(30,40,30),
             spec_freqRange = c(0,16000),
             mainTitle='Defining the IOI with an example from Dolgan',
             #annotate =c(4,0.22,0.9, font=6),
             tg_tiers = c('text-dolgan', 'text-english', 'word-dolgan','word-english'),
             tg_color = c('blue', 'black', 'black', 'black'),
             tg_focusTier = 'all',
             tg_focusTierLineType = c('solid', 'solid', 'dashed', 'dashed'),
             tg_focusTierColor = c('black', 'black', 'black','black'),
             draw_arrow = c('spectrogram', 0.478, 8500, 2.321, 8500, code = 3,
                            length = 0.15, 
                            angle = 20, col = 'blue', lwd = 2, lty = 'solid'),
             annotate = c('spectrogram', 1.05, 7000, col = 'blue', font=2, cex = 1.8,labels = 'Inter-Onset-Interval (IOI)'))


