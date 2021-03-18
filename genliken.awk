#! /usr/bin/awk -f

# cmnd line:
# samtools view -F 4079 align.sort.sam  | awk '{ printf("%s, %s, %s, %s, %s, Read Length: %s",$1,$2,$3,$4,$5,length($10)); /
# n = split($6,arr,"[0-9]+[IDSH]"); sum=0; /
# for(i=0;i<=n;i++) { sum += substr(arr[i],1,length(arr[i]-1)) } m = split ($6,arr,"[0-9]+[MIDNP=X]"); / 
# clip=0; for(i=0;i<=m;i++) { clip += substr(arr[i],1,length(arr[i]-1)) }  /
# printf(" Match: %s, Coverage %s, Coverage(less Clip): %s :  %s  \n",sum,(sum/length($10)),clip,(sum/(length($10)-clip))) }' 
#


{
    # pipe in sam file reads 
printf("+++++ %s, %s %s, %s, %s, Read Length:%s. ",$1,$2,$3,$4,$5,length($10)); 
n = split($6,arr,"[0-9]+[IDSH]");  # split out matching cigars (M also X and =)
# MATCH
matchSum=0; 
for(i=1;i<=n;i++) { 
    cnv = substr(arr[i],1,length(arr[i])-1)  # sum the lengths of M (X,=) cigars
    matchSum += cnv
    # TEST 
    # arril = length(arr[i])
    # printf ("%s:%d:%s:%s:%s\t",arr[i],arril,substr(arr[i],1,arril-1),substr(arr[i],1,length(arr[i])-1),cnv)
    # if ((i % 10) == 0 ) print ""
}
p = split ($6,arr,"[0-9]+[MISHNP=X]");  # find d
# DELETE
cDel=0; 
bigDel="";
for(i=1;i<=p;i++) { 
    cnv = substr(arr[i],1,length(arr[i])-1)  # sum the lengths of D cigars
    if ( ( cnv + 1  ) > 45 ) {            # + 1 to force a numeric comparision?
            bigDel = bigDel arr[i] ":"
           # print bigDel
    }
    cDel += cnv
}    
q = split ($6,arr,"[0-9]+[MDSHNP=X]"); # find I
# INSERT
cIns=0; 
bigIns="";
for(i=1;i<=q;i++) { 
    cnv = substr(arr[i],1,length(arr[i])-1)  # sum the lengths of I cigars
    if ( ( cnv + 1) > 45 ) {    # + 1 to force a numeric comparision? 
            bigIns =  bigIns arr[i] ":" 
          #  printf("\n%s %s,",cnv,bigIns)
    }    
    cIns += cnv
}    
r = split ($6,arr,"[0-9]+[MIDSHNP=]");  # find x
# MISMATCH
cMis=0; 
for(i=1;i<=r;i++) { 
    cnv = substr(arr[i],1,length(arr[i])-1)  # sum the lengths of X cigars
    cMis += cnv
}

m = split ($6,arr,"[0-9]+[MIDNP=X]");   # sum the length of S soft and H hard clippings = clip
# CLIP
clip=0; 
printf("Cigar Elements(no clip):%d. (Clips:",m)
for(i=1;i<=m;i++) { 
    clip += substr(arr[i],1,length(arr[i])-1) 
    if ( length(arr[i]) > 0 ) printf("%s ",arr[i]) 
} 
printf(")")
# $10 is sequence being aligned, sum = matched to genome/ref and clip = size of clipped read (ends)
# coverage is sum/length($10)  Coverage less clip is Matches per aligned region (ie less Indels)
printf(" Ins:%s, Del:%s, Miss:%s, Match:%s, Coverage:%s, Coverage(less Clip:%s):%s\n",cIns,cDel,cMis,matchSum,(matchSum/length($10)),clip,(matchSum/(length($10)-clip))) 
printf("indel greater than 45 in current read: Large Ins: %s. Large Del: %s.\n",bigIns,bigDel) 
#   TESTING>>>>>>>>>>
# x = split($6,arr,"[0-9]+")
# y = split($6,brr,"[A-Z]")
# z = 1
# print "TEST"
# for(i=1;i<=x;i++) { 
#     if ( arr[i+1] == "M" ) {
#             printf("%s\t", brr[i], arr[i+1])  # sum the lengths of M (X,=) cigars
#             z = z + 1
#         if ((z % 10) == 0 ) print ""
#     }
# } 



# comnand to check clipping:
# samtools view -F 4079 align.sort.sam | head -n2 | awk '{ print $1,$2,$3,$4,$5,substr($6,1,20),"...",substr($6,length($6)-20,length($6)) }'

# Command: samtools view -F 4079 align.sort.sam | awk -f statAlign.awk | less -S
# -F 4079 will only ouput primary alignments (flag 0 and 16)

# NEED to FIX Hard clips dont go into calc for % correct (As they are no in sequence in sam file)



# genliken:

regex =  "[[:upper:]]+"; 
n=split($6, arr, regex);
regex =  "[[:digit:]]+";
m=split($6, brr, regex);
pos=0
read_seq= ""    # not really used (leftover form past code) builds a representation of read...
softClip = ""
hardClip = ""
startsoftclip = 1
posStr = ""
rlen = 1
mStart = 0
hcStart = 0
scStart = 0
mEnd = 0
hcEnd = 0
scEnd = 0

                scFStart = 0
                scFEnd = 0
                scRStart = 0
                scREnd = 0
                hcFEnd = 0
                hcFStart = 0
                hcRStart = 0
                hcREnd = 0
                iSV = ""
                dSV = ""

for ( i=1; i<n; i++ ) {   
	# print arr[i] ":" brr[i+1]
	len = arr[i]

	switch( brr[i+1] ) {
	
	case "M" : read_seq = read_seq substr(seq,pos,len)
            if (mStart == 0) { mStart = pos + $4 }     # $4 is position
			pos = pos + len
			addPos = 1
			break;
        case "=" : read_seq = read_seq substr(seq,pos,len)
                        if (mStart == 0) { mStart = pos + $4 + 1 }
                        pos = pos + len
                        addPos = 1
                        break;
        case "X" : read_seq = read_seq substr(seq,pos,len)
                        if (mStart == 0) { mStart = pos + $4 + 1 }
                        pos = pos + len
                        addPos = 1
                        break;
	case "D" : for(c=0;c<len;c++) read_seq = read_seq "-"
                        if ( len > 45 ) { 
                            if ( i > 1) { dSV = dSV ", " }
                            dSV = dSV (pos + $4) ", "  (-len)  
                        }
                        pos = pos + len   # hmm ?  to do add tick mark ?? 
                        addPos = 1
			break;
        case "I" : for(c=0;c<len;c++) read_seq = read_seq ":"
                      #  pos = pos + len   dont count insert   to do add tick mark
                    if ( len > 45 ) { 
                        if ( i > 1) { iSV = iSV ", " }
                        iSV = iSV (pos + $4) ", "  (len) 
                    }
                       break;
        case "N" : for(c=0;c<len;c++) read_seq = read_seq "N"
                        pos = pos + len
                        break;
        case "P" : for(c=0;c<len;c++) read_seq = read_seq "P"
                        pos = pos + len
                        break;
        case "S" : softClip = softClip "S" len " "
        # print softClip
			if ( pos == 0 ) {
				startsoftclip = len
				rlen = len
                scFStart = $4
                scFEnd = $4 + len
			} else {
                mEnd = pos + $4   # if pos not 0 then at end of cigar and last element is S if here
                scRStart = mEnd + 1 
                scREnd = pos + len + $4 
            }
                        pos = pos + len
                        break;
        case "H" : hardClip = hardClip "H" len " "
        # print hardClip
        			if ( pos == 0 ) { 
                        hcFEnd = $4
                        hcFStart = $4 - len 
                    } else { 
                        mEnd = pos + $4
                        hcRStart = mEnd + 1
                        hcREnd = pos + len + $4 
                        
                    }
                        # pos does not alter for hard clip 
                        break;

	default:
		break;

	}
}

if (mEnd == 0) { mEnd = $4 + pos }   # if no trailing clips need to set end of matching area

#    printf("#### cigar :%s\n ------- region %s, Clip Soft Front %s, %s Clip Hard Front %s, %s Match  %s, %s \n",  \
#    $6,$4,scFStart,scFEnd,hcFStart,hcFEnd,mStart,mEnd)
#    printf("#### Clip Soft Rear %s,%s  Clip Hard Rear %s,%s. \n",  \
#    scRStart,scREnd,hcRStart,hcREnd) 

    printf("%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s",  \
    $4,scFStart,scFEnd,hcFStart,hcFEnd,mStart,mEnd,scRStart,scREnd,hcRStart,hcREnd,$1) > "genliken.csv"
    if ( length(dSV) > 1 ) { printf("%s",dSV) > "genliken.csv" }
    if ( length(iSV) > 1 ) { printf("%s",iSV) > "genliken.csv" }
    printf ("\n") > "genliken.csv"
}
