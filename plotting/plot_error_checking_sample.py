# An example to check false alerts through plotting the SG trends

import pandas as pd
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import time
from os.path import join, expanduser

def plot_error(sglist, epochlist, scorelist, scoreepoch, title, outfile, hours_before, hours_after):
    plt.close('all')
    matplotlib.rcParams['figure.figsize'] = (12, 10)
    #plt.figure(figsize=(100,50))

    fig, (ax1, ax2) = plt.subplots(nrows=2, ncols=1)

    sgs = pd.DataFrame(data=sglist, index=epochlist)
    sgs.plot(legend=None,ax=ax1)

    ax1.set_ylabel('SG')
    ax1.set_title(title)
    ax1.set_ylim(0,300)
    yoffset = 250 #for text display
    xoffset = 200 * hours_before #for text display

    ax1.axvline(x=epoch_anchor, color='C1')
    ax1.text(epoch_anchor-xoffset,yoffset,'current epoch',rotation=90)

    #print("hours_before:%s, hours_after:%s" %(hours_before, hours_after))
    for h in range(1,hours_before):
        if epoch_anchor-3600*h > epochlist[0]:
            ax1.axvline(x=epoch_anchor-3600*h, color='C%s'%h, ls='dashdot')
            ax1.text(epoch_anchor-3600*h-xoffset,yoffset,'%shr before'%h,rotation=90)

    for h in range(1,hours_after):
        if epoch_anchor+3600*h < epochlist[len(epochlist)-1]:
            ax1.axvline(x=epoch_anchor+3600*h, color='C%s'%h, ls='dashdot')
            ax1.text(epoch_anchor+3600*h-xoffset,yoffset,'%shr after'%h,rotation=90)

    ax1.axhline(y=70, color='C8')
    ax1.text(epoch_anchor-3600*hours_before,74,'hypo threshold=70')

    ax1.tick_params(
    axis='x',          # changes apply to the x-axis
    which='both',      # both major and minor ticks are affected
    bottom='off',      # ticks along the bottom edge are off
    top='off',         # ticks along the top edge are off
    labelbottom='off') # labels along the bottom edge are off

    scores = pd.DataFrame(data=scorelist, index=scoreepoch)
    scores.plot(legend=None,ax=ax2)

    ax2.set_ylabel('Risk score')
    ax2.set_title(title)
    #ax2.set_ylim(0,1.1)
    #yoffset = 250 #for text display
    #xoffset = 200 * hours_before #for text display

    ax2.axvline(x=epoch_anchor, color='C1')

    #print("hours_before:%s, hours_after:%s" %(hours_before, hours_after))
    for h in range(1,hours_before):
        if epoch_anchor-3600*h > scoreepoch[0]:
            ax2.axvline(x=epoch_anchor-3600*h, color='C%s'%h, ls='dashdot')

    for h in range(1,hours_after):
        if epoch_anchor+3600*h < scoreepoch[len(scoreepoch)-1]:
            ax2.axvline(x=epoch_anchor+3600*h, color='C%s'%h, ls='dashdot')

    plt.savefig(outfile,bbox_inches='tight')
    #plt.clf()
    #plt.show()

freq_start=0.0
freq_end=1.0
experimentdir = "/homes/hny9/yuanchi/Minimed/Exp6Expanded/"
experimentfile = experimentdir + "Run-"+str(freq_start)+"-"+str(freq_end)+"-2hr-10/predictions/train80_test20_label2hr_r100_predictions.csv"
featuredir="/storage2/medtronics/onethousandusers-01252018/onethousand_csv_SG_features_all/"

#experimentdir = "/Users/yuanchi/Desktop/Jupyter/Minimed/"
#experimentfile = experimentdir + "train80_test20_label2hr_r100_predictions.csv"
#featuredir=experimentdir
outdir="/homes/hny9/yuanchi/Minimed/ErrorPlot/"

plt.close('all')

pred_user_df = pd.read_csv(experimentfile)
pred_user_df_hypo = pred_user_df.loc[pred_user_df['y_test'] == 0]
#sort from large to small
pred_user_df_hypo = pred_user_df_hypo.sort_values(['y_pred'],ascending=False)
#pred_user_df_hypo.head()

userid_epoch_false_pos = pred_user_df_hypo.head(1000)
#print(userid_epoch_false_pos)
hours_before=5
hours_after=5
case = "FalsePositive_"

dedup = []
prev_userid = None
df = None
sglist = None
scorelist = None
scoreepoch = None
epochlist = None
for index, row in userid_epoch_false_pos.iterrows():
    #print(row['userid'], row['epoch'], row['y_test'], row['y_pred'])
    userid = int(row['userid'])
    epoch_anchor = int(row['epoch'])
    dedupkey = str(userid)+time.strftime('%Y-%m-%d', time.localtime(epoch_anchor))
    if dedupkey in dedup:
	continue
    dedup.append(dedupkey)
    #print(userid, epoch_anchor)
    epoch_start = epoch_anchor - 3600 * hours_before
    epoch_end = epoch_anchor + 3600 * hours_after
    if prev_userid != userid:
        filename = join(featuredir,str(userid)+"_combine.csv")
        #print(filename)
        df = pd.read_csv(filename)
        df = df.loc[(df['epoch'] >= epoch_start) & (df['epoch'] <= epoch_end)][['epoch','sglatest']]
        sglist = list(df['sglatest'])
        epochlist = list(df['epoch'])

        scoredf = pred_user_df.loc[(pred_user_df['epoch'] >= epoch_start) & (pred_user_df['epoch'] <= epoch_end) & (pred_user_df['userid'] == userid)][['epoch','y_pred']]
        scorelist = list(scoredf['y_pred'])
        scoreepoch = list(scoredf['epoch'])

        prev_userid = userid

    title="%s id:%s epoch:%s user average weekly hypo between %f and %f" %(case,userid,epoch_anchor,freq_start,freq_end)
    outfile = outdir+"%sid%s_epoch%s_ytest%s_ypred%s_before%sh_after%sh.png"%(case,userid,epoch_anchor, row['y_test'], row['y_pred'], hours_before, hours_after)
    plot_error(sglist, epochlist, scorelist, scoreepoch, title, outfile, hours_before, hours_after)
