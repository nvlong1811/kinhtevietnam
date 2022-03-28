#------------ PHAN TICH DU LIEU PHAT TRIEN CUA VIET NAM TU NAM 1960 DEN NAY ------------
#1: ------------ Chon dataset ------------

rm(list=ls())
library("dplyr")
library(ggplot2)
PATH <- "C:\\Users\\Longddz\\Desktop\\DataAnalyst\\VietNamNetTopModel\\vietnam.csv"
PathDescription <- "C:\\Users\\Longddz\\Desktop\\DataAnalyst\\VietNamNetTopModel\\vietnam_date.csv"
vietnam_fully <- "C:\\Users\\Longddz\\Desktop\\DataAnalyst\\VietNamNetTopModel\\vietnam_fully.csv"
data <- read.csv(PATH, header = TRUE, sep = ",")
data_date <- read.csv(PathDescription, header = TRUE, sep = ",")
data_fully <- read.csv(vietnam_fully, header = TRUE, sep = ",")

# ------------ Tien xu ly du lieu ---------------------
#nhung chi so can loai bo: 
#SE.SEC.ENRR
#SE.ENR.PRSC.FM.ZS
#GC.REV.XGRT.GD.ZS
#GC.DOD.TOTL.GD.ZS
#ER.H2O.FWTL.ZS
#IC.BUS.EASE.XQ
#IC.REG.DURS
#SM.POP.NETM
#SP.DYN.CONU.ZS
#SH.STA.BRTC.ZS
#SE.PRM.ENRR
#SE.PRM.CMPT.ZS

colnames<-c("SE.SEC.ENRR","SE.ENR.PRSC.FM.ZS","GC.REV.XGRT.GD.ZS","GC.DOD.TOTL.GD.ZS",
            "ER.H2O.FWTL.ZS","IC.BUS.EASE.XQ","IC.REG.DURS","SM.POP.NETM","SP.DYN.CONU.ZS",
            "SH.STA.BRTC.ZS","SE.PRM.ENRR","SE.PRM.CMPT.ZS")
data<-select(data,-colnames)


#2: ------------ GIai thich cac bien cua dataset ------------
#	TX.VAL.TECH.MF.ZS	:	San luong xuat khau cong nghe cao (% san luong xuat khau san xuat)
#	TT.PRI.MRCH.XD.WD	:	Chi so ty le thuong mai hang hoa rong (2000 = 100)
#	TG.VAL.TOTL.GD.ZS	:	Thuong mai hang hoa (% GDP)
#	SP.POP.TOTL	:	Dan so, tong cong
#	SP.POP.GROW	:	Tang truong dan so (% hang nam)
#	SP.DYN.TFRT.IN	:	Ty le sinh san, tong cong (so lan sinh san cua moi mot phu nu)
#	SP.DYN.LE00.IN	:	Tuoi tho uoc tinh vao thoi diem chao doi, tong cong (so tuoi)
#	SP.DYN.IMRT.IN	:	Ty le tu vong, tre so sinh (tren moi 1,000 tre em con song khi chao doi)
#	SP.DYN.CONU.ZS	:	Ty le tranh thai (% phu nu tu 15-49 tuoi)
#	SP.ADO.TFRT	:	Ty le sinh san o thanh thieu nien (so ca sinh no tren moi 1,000 phu nu tu 15-19 tuoi)
#	SM.POP.NETM	:	Nhap cu thuan tuy
#	SL.UEM.TOTL.ZS	:	That nghiep, tong cong (% tong so luc luong lao dong)
#	SI.POV.NAHC	:	Ty le ngheo doi theo dau nguoi o muc ngheo doi quoc gia (% dan so)
#	SI.POV.GINI	:	Chi so GINI
#	SI.POV.DDAY	:	Ty le nguoi ngheo tinh theo dau nguoi o muc chi tieu $1.90 mot ngay (PPP) (% dan so)
#	SI.DST.FRST.20	:	Phan thu nhap cua 20% thap nhat
#	SH.STA.MALN.ZS	:	Ty le suy dinh duong, can nang theo tuoi (% tre em duoi 5 tuoi)
#	SH.STA.BRTC.ZS	:	So ca sinh no co nhan vien y te co tay nghe tham gia (% tong so)
#	SH.IMM.MEAS	:	Tiem phong, benh soi (% tre em tu 12-23 thang tuoi)
#	SH.DYN.MORT	:	Ty le tu vong duoi 5 tuoi (moi 1,000 tre em con song khi chao doi)
#	SH.DYN.AIDS.ZS	:	Ty le nhiem HIV, tong cong (% dan so tu 15-49 tuoi)
#	SE.XPD.TOTL.GD.ZS	:	Muc chi cua chinh phu cho giao duc, tong cong (% GDP)
#	SE.SEC.ENRR	:	Si so hoc sinh, trung hoc (% tong so)
#	SE.PRM.ENRR	:	Si so hoc sinh, tieu hoc (% tong so)
#	SE.PRM.CMPT.ZS	:	Ty le hoan thanh tieu hoc, tong cong (% nhom tuoi lien quan)
#	SE.ENR.PRSC.FM.ZS	:	Ty le hoc sinh nu tren hoc sinh nam o cap giao duc tieu hoc va trung hoc (%)
#	SE.ADT.LITR.ZS	:	Ty le biet chu, tong cong nguoi lon (% so nguo tu 15 tuoi tro len)
#	NY.GNP.PCAP.PP.CD	:	GNI tinh theo dau nguoi, PPP ($ quoc te hien tai)
#	NY.GNP.PCAP.CD	:	GNI theo dau nguoi, phuong phap Atlas (US$ hien tai)
#	NY.GNP.MKTP.PP.CD	:	GNI, PPP ($ quoc te hien tai)
#	NY.GNP.ATLS.CD	:	GNI, phuong phap Atlas (US$ hien tai)
#	NY.GDP.PCAP.PP.CD	:	GDP tinh theo dau nguoi, PPP ($ quoc te hien tai)
#	NY.GDP.PCAP.CD	:	GDP tinh theo dau nguoi (US$ hien tai)
#	NY.GDP.MKTP.KD.ZG	:	Tang truong GDPâ(% hang nam)
#	NY.GDP.MKTP.CD	:	GDP (US$ hien tai)
#	NY.GDP.DEFL.KD.ZG	:	Lam phat, yeu to giam phat GDP (% hang nam)
#	NV.IND.TOTL.ZS	:	Cong nghiep, gia tri gia tang (% GDP)
#	NV.AGR.TOTL.ZS	:	Nong nghiep, gia tri gia tang (% GDP)
#	NE.IMP.GNFS.ZS	:	Kim ngach nhap khau hang hoa va dich vu (% GDP)
#	NE.GDI.TOTL.ZS	:	Tong tich luy tai san (% GDP)
#	NE.EXP.GNFS.ZS	:	Kim ngach xuat khau hang hoa va dich vu (% GDP)
#	MS.MIL.XPND.GD.ZS	:	Chi ngan sach cho quoc phong (% GDP)
#	IT.CEL.SETS.P2	:	Luong thue bao dien thoai di dong (moi 100 nguoi)
#	IC.REG.DURS	:	Thoi gian bat buoc de khoi su kinh doanhâ(ngay)
#	IC.BUS.EASE.XQ	:	Chi so thuan loi hoat dong kinh doanh (1=cac qui che tao dieu kien thuan loi nhat cho hoat dong kinh doanh)
#	GC.REV.XGRT.GD.ZS	:	Tong thu ngan sach, khong ke cac khoan tro cap (% GDP)
#	GC.DOD.TOTL.GD.ZS	:	No chinh phu trung uong, tong cong (% GDP)
#	GB.XPD.RSDV.GD.ZS	:	Khoan chi cho nghien cuu va phat trien (% GDP)
#	FP.CPI.TOTL.ZG	:	Lam phat, gia tieu dung (% hang nam)
#	ER.H2O.FWTL.ZS	:	Luong nuoc ngot lay ra hang nam, tong cong (% nguon luc ben trong)
#	EN.ATM.CO2E.PC	:	Luong khi thai CO2 (met tan tren dau nguoi)
#	EG.USE.PCAP.KG.OE	:	Su dung nang luong (kg dau tuong duong tinh theo dau nguoi)
#	EG.USE.ELEC.KH.PC	:	Tieu dung dien (kWh tren dau nguoi)
#	DT.TDS.DECT.EX.ZS	:	Tong nghia vu tra no (% doanh so xuat khau hang hoa, dich vu va thu nhap can ban)
#	DT.ODA.ODAT.CD	:	Tro cap phat trien chinh thuc rong nhan duoc (US$ hien tai)
#	DT.DOD.DECT.CD	:	Tong du no nuoc ngoai (DOD, US$ hien hanh)
#	CM.MKT.LCAP.GD.ZS	:	Gia tri thi truong cua cac cong ty niem yet tren thi truong chung khoan (% of GDP)
#	BX.TRF.PWKR.CD.DT	:	Tien chuyen khoan ca nhan, nhan duoc (US$ hien tai)
#	BX.KLT.DINV.CD.WD	:	Dau tu truc tiep cua nuoc ngoai, luong dau tu thuan tuy (BoP, US$ hien tai)
#	AG.SRF.TOTL.K2	:	Dien thich be mat (km vuong)
#	AG.LND.FRST.K2	:	Dien tich lam nghiep (km vuong)
#	AG.LND.AGRI.ZS	:	Dat nong nghiep (% dien tich dat)
