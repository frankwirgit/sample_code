# An example for summary statistical analysis based on patient demographics info
# gender, race, bmi etc.

import numpy as np
import scipy as sp
import scipy.stats

from pandas import *
import math
import sys
reload(sys)
sys.setdefaultencoding('utf8')
unicode('\xd0', errors='ignore')
import matplotlib.pyplot as plt
from matplotlib.pylab import rcParams
rcParams['figure.figsize'] = 14,5

def plothistogram(alphab,frequencies,title):
    pos = np.arange(len(alphab))
    width = 1  # gives histogram aspect to the bar diagram

    ax = plt.axes()
    ax.set_xticks(pos + (width / 2))
    ax.set_xticklabels(alphab)
    plt.title(title)
    plt.bar(pos, frequencies, width, color='blue')
    plt.show()
def percentage(x,y):
    return ((int(x)*int(100))/float(y))

if __name__ == "__main__":
    #df = pandas.read_stata('samadult.dta', encoding= 'utf-8')
    df = pandas.read_stata('Data\samadult2014.dta', encoding= 'utf-8')
    kneepainlistright = df['jmthp9'].tolist()
    print df['vigfreqw'].value_counts()
    kneepainlistleft = df['jmthp10'].tolist()
    statuslist = df['r_maritl'].tolist()
    weightlist = df['wtfa_sa'].tolist()
    agelist = df['age_p'].tolist()
    sexlist = df['sex'].tolist()
    racelist=df['racerpi2'].tolist()
    hispaniclist= df['hispan_i'].tolist()
    arthlist= df['arth1'].tolist()
    arthwt=df['arthwt'].tolist()
    arthph=df['arthph'].tolist()
    bmi=df['bmi'].tolist()
    crudekneepaincount = 0
    totalweight = 0
    malecount=0
    femalecount=0
    malefeaturecount=0
    femalefeaturecount = 0
    bmicnt=0
    for i in xrange(len(weightlist)):
        totalweight += weightlist[i]
        #gender analysis
        if sexlist[i] == 1:
            malecount+=weightlist[i]
            if ((kneepainlistright[i] == 1) or (kneepainlistleft[i] == 1)):
                malefeaturecount+=weightlist[i]
        elif sexlist[i] == 2:
            femalecount+=weightlist[i]
            if ((kneepainlistright[i] == 1) or (kneepainlistleft[i] == 1)):
                femalefeaturecount +=weightlist[i]



        if ((kneepainlistright[i]==1) or (kneepainlistleft[i]==1)):
            crudekneepaincount += weightlist[i]

    #print "\nCrude percent : " + str((percentage(crudeweightcount, totalweight)))

    print '\n********* Gender analysis *****************\n'
    print str(percentage(malefeaturecount, crudekneepaincount)) + ' Male among total knee pain affected%\n'
    print str(percentage(femalefeaturecount, crudekneepaincount)) + ' Female perentage among total knee pain affected %\n'

    listM = ['Men', 'Women']
    listW = [percentage(malefeaturecount,malecount), percentage(femalefeaturecount, femalecount)]
    plothistogram(listM, listW, 'Percentage of poeple affected by knee pain according to gender')

    #print str(percentage(malefeaturecount, totalweight)) + ' Male among all%\n'
    #print str(percentage(femalefeaturecount, totalweight)) + ' Female perentage among all%\n'

    #print str(percentage(malefeaturecount, malecount)) + ' Male among males%\n'
    #print str(percentage(femalefeaturecount, femalecount)) + ' Female perentage among females%\n'
    print '\n********* Gender analysis Ends *****************\n'

    print '\n********* Race analysis *****************\n'
    arthcount=0
    arthwtcnt=0
    arthex=0
    regionlist = df['region'].tolist()
    northeast = 0
    midwest = 0
    south = 0
    west = 0
    countHispanicwithkneepain,countwhitewithkneepain,countblackwithkneepain,\
    countAIwithkneepain,countAsianwithkneepain,countMultipleracewithkneepain,hispcnt,whtcnt,blckcnt,AIcnt,asiancnt,multcnt=0,0,0,0,0,0,0,0,0,0,0,0
    cnt,cntsmkeknee,cntinactiveknee,cntalcknee,cntotherjointpn,cnthyper=0,0,0,0,0,0
    private,govt,selfe,totalp,totalg,totals,cntdepress,married,single=0,0,0,0,0,0,0,0,0
    totalnortheast,totalmidwest,totalsouth,totalwest=0,0,0,0
    for i in xrange(len(weightlist)):
        if regionlist[i] == 1:
            totalnortheast += weightlist[i]
        elif regionlist[i] == 2:
            totalmidwest += weightlist[i]
        elif regionlist[i] == 3:
            totalsouth += weightlist[i]
        elif regionlist[i] == 4:
            totalwest += weightlist[i]
        if (df['wrkcata'].tolist()[i] == 1):
            totalp += weightlist[i]
        elif (df['wrkcata'].tolist()[i] in [2, 3, 4]):
            totalg += weightlist[i]
        elif (df['wrkcata'].tolist()[i] in [5, 6]):
            totals += weightlist[i]
        #race analysis
        if ((kneepainlistright[i] == 1) or (kneepainlistleft[i] == 1)):
            if (statuslist[i] in [1, 2, 3]):
                married += 1
            else:
                single += 1

            if arthlist[i]==1:
                arthcount+=weightlist[i]

            if arthwt[i]==1:
                arthwtcnt+=weightlist[i]

            if arthph[i] == 1:
                arthex += weightlist[i]

            cnt+=1
            bmicnt+=bmi[i]

            if df['smkstat2'].tolist()[i] in [1,2,3,5]:
                cntsmkeknee+=weightlist[i]
            if df['asiwthls'].tolist()[i] in [1,2,3,4]:
                cntdepress+=weightlist[i]

            if((df['modfreqw'].tolist()[i] in [0,1,95,96]) or (df['vigfreqw'].tolist()[i] in [0,1,95,96])):
                cntinactiveknee+=weightlist[i]

            if df['alcstat'].tolist()[i] in [7,8]:
                cntalcknee+=weightlist[i]

            if regionlist[i]==1:
                northeast+=weightlist[i]
            elif regionlist[i]==2:
                midwest+=weightlist[i]
            elif regionlist[i]==3:
                south+=weightlist[i]
            elif regionlist[i]==4:
                west+=weightlist[i]

            if (df['hypev'].tolist()[i]==1):
                cnthyper+=weightlist[i]

            if ((df['jmthp1'].tolist()[i]==1) or (df['jmthp2'].tolist()[i]==1) or (df['jmthp3'].tolist()[i]==1)or (df['jmthp4'].tolist()[i]==1)
                or (df['jmthp5'].tolist()[i]==1)or (df['jmthp6'].tolist()[i]==1)or (df['jmthp7'].tolist()[i]==1)or (df['jmthp8'].tolist()[i]==1)
                or (df['jmthp11'].tolist()[i]==1)or (df['jmthp12'].tolist()[i]==1)or (df['jmthp13'].tolist()[i]==1)or (df['jmthp14'].tolist()[i]==1)or (df['jmthp15'].tolist()[i]==1)or (df['jmthp16'].tolist()[i]==1)or (df['jmthp17'].tolist()[i]==1)):
                cntotherjointpn+=weightlist[i]


            if (df['wrkcata'].tolist()[i]==1):
                private+=weightlist[i]
            elif (df['wrkcata'].tolist()[i] in [2,3,4]):
                govt+=weightlist[i]
            elif (df['wrkcata'].tolist()[i] in [5,6]):
                selfe+=weightlist[i]



        if hispaniclist[i]<12:
            hispcnt+=weightlist[i]
            if ((kneepainlistright[i] == 1) or (kneepainlistleft[i] == 1)):
                countHispanicwithkneepain+=weightlist[i]

        if racelist[i]==1:
            whtcnt += weightlist[i]
            if ((kneepainlistright[i] == 1) or (kneepainlistleft[i] == 1)):
                countwhitewithkneepain += weightlist[i]
        if racelist[i] == 2:
            blckcnt += weightlist[i]
            if ((kneepainlistright[i] == 1) or (kneepainlistleft[i] == 1)):
                countblackwithkneepain += weightlist[i]
        elif racelist[i] == 3:
            AIcnt += weightlist[i]
            if ((kneepainlistright[i] == 1) or (kneepainlistleft[i] == 1)):
                countAIwithkneepain += weightlist[i]
        elif racelist[i] == 4:
            asiancnt += weightlist[i]
            if ((kneepainlistright[i] == 1) or (kneepainlistleft[i] == 1)):
                countAsianwithkneepain += weightlist[i]
        elif racelist[i] == 6:
            multcnt += weightlist[i]
            if ((kneepainlistright[i] == 1) or (kneepainlistleft[i] == 1)):
                countMultipleracewithkneepain += weightlist[i]

    print '\npeople with knee pain suffering from depression: ' + str(percentage(cntdepress, crudekneepaincount))
    print '\npeople with knee pain working in private jobs: ' + str(percentage(private, crudekneepaincount))
    print '\npeople with knee pain working in government jobs: ' + str(percentage(govt, crudekneepaincount))
    print '\npeople with knee pain working in self employed jobs: ' + str(percentage(selfe, crudekneepaincount))
    print str(percentage(private, totalp)),str(percentage(govt, totalg))

    listJob = ['Private jobs', 'Government jobs', 'Self employed']
    listval = [percentage(private, df['wrkcata'].tolist().count(1)), percentage(govt, df['r_maritl'].tolist().count(2)+df['r_maritl'].tolist().count(3)+df['r_maritl'].tolist().count(4)),percentage(selfe, df['r_maritl'].tolist().count(5))]
    plothistogram(listJob, listval, 'Percentage of poeple affected by knee pain according to job type')

    maritallist = ['Married', 'Single']
    listval2 = [percentage(married, df['r_maritl'].tolist().count(1)+df['r_maritl'].tolist().count(2)+df['r_maritl'].tolist().count(3)), percentage(single, df['r_maritl'].tolist().count(4)+df['r_maritl'].tolist().count(5)+df['r_maritl'].tolist().count(6)+df['r_maritl'].tolist().count(7))]
    plothistogram(maritallist, listval2, 'Percentage of poeple affected by knee pain according to marital status')


    listregion = ['NorthEast', 'Midwest', 'South', 'West']
    lstval = [percentage(northeast, totalnortheast), percentage(midwest, totalmidwest),percentage(south, totalsouth),percentage(west, totalwest)]
    plothistogram(listregion, lstval, 'Percentage of poeple affected by knee pain according to region')

    print '\nPerc of people with other joint pain' + str(percentage(cntotherjointpn, crudekneepaincount))
    print '\nPerc of drinkers in knee affected people' + str(percentage(cntalcknee,crudekneepaincount))

    print '\nNumber of people inactive and have knee pain'+ str(percentage(cntinactiveknee,crudekneepaincount))
    print '\nNumber of knee patients who smoke and have knee pain'+ str(percentage(cntsmkeknee,crudekneepaincount))

    print str(bmicnt/cnt) + 'is the average bmi of knee pain patients\n'
    print arthcount,arthwtcnt,arthex,crudekneepaincount
    print str(percentage(arthcount, crudekneepaincount)) + ' people affected with arthritis among total knee pain affected%\n'
    print str(percentage(arthwtcnt, crudekneepaincount)) + ' people advised to lose weight%\n'
    print str(percentage(arthex, crudekneepaincount)) + ' people advised to exercise%\n'

    print str(percentage(countHispanicwithkneepain, crudekneepaincount)) + ' Hispanics among total knee pain affected%\n'
    print str(percentage(countwhitewithkneepain, crudekneepaincount)) + ' Whites among total knee pain affected%\n'
    print str(percentage(countblackwithkneepain, crudekneepaincount)) + ' Blacks among total knee pain affected%\n'
    print str(percentage(countAIwithkneepain, crudekneepaincount)) + ' American Indians among total knee pain affected%\n'
    print str(percentage(countAsianwithkneepain, crudekneepaincount)) + ' Asian among total knee pain affected%\n'
    print str(percentage(countMultipleracewithkneepain, crudekneepaincount)) + ' Multiple races among total knee pain affected%\n'


    listrace = ['Hispanics', 'Whites', 'Blacks', 'American Indians+Asians', 'Multiple races']
    lstval1 = [percentage(countHispanicwithkneepain, hispcnt), percentage(countwhitewithkneepain, whtcnt),percentage(countblackwithkneepain, blckcnt),percentage(countAIwithkneepain+countAsianwithkneepain, AIcnt+asiancnt),percentage(countMultipleracewithkneepain, multcnt)]
    plothistogram(listrace, lstval1, 'Percentage of poeple affected by knee pain according to race')


    print str(percentage(countHispanicwithkneepain, totalweight)) + ' Hispanics among all%\n'
    print str(percentage(countwhitewithkneepain, totalweight)) + ' Whites among all%\n'
    print str(percentage(countblackwithkneepain, totalweight)) + ' Blacks all%\n'
    print str(percentage(countAIwithkneepain, totalweight)) + ' American Indians among all %\n'
    print str(percentage(countAsianwithkneepain, totalweight)) + ' Asian among all%\n'
    print str(percentage(countMultipleracewithkneepain, totalweight)) + ' Multiple races among all%\n'


    print str(percentage(countHispanicwithkneepain, hispcnt)) + ' Hispanics among hispanics%\n'
    print str(percentage(countwhitewithkneepain, whtcnt)) + ' Whites among whites%\n'
    print str(percentage(countblackwithkneepain, blckcnt)) + ' Blacks among blacks%\n'
    print str(percentage(countAIwithkneepain, AIcnt)) + ' American Indians among Ais %\n'
    print str(percentage(countAsianwithkneepain, asiancnt)) + ' Asian among asians%\n'
    print str(percentage(countMultipleracewithkneepain, multcnt)) + ' Multiple races among Mraces%\n'





    print '\n********* Race analysis Ends *****************\n'

