################1,3 beta glucan synthase inter


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	inter	CvsDi	TRINITY_DN103194_c0_g2	10.62458061	6.878436828	3.636470911	1.891514327	0.05855572	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN713865_c0_g1	61.91721377	-9.562063664	3.489260667	-2.740426863	0.006135944	0.084430584	down_regulated
stress	inter	CvsDi	TRINITY_DN123998_c0_g1	11.12337825	-7.086181154	3.619747842	-1.957644969	0.050271687	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN199459_c0_g1	11.92593303	7.188434478	3.156014802	2.277693525	0.022744844	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN713865_c0_g1	52.60742089	-9.036038536	3.574937871	-2.527607153	0.011484277	0.09027918	down_regulated
stress	inter	CvsDr	TRINITY_DN36313_c0_g1	29.13814435	5.423208905	2.698208867	2.009929243	0.044438678	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN103194_c0_g2	19.15692921	7.994348427	3.882197129	2.059232996	0.039471924	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1119070_c0_g1	22.86050876	8.248946442	3.877830892	2.127206336	0.033402943	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN713865_c0_g1	52.59265389	-8.813615764	3.899802371	-2.260016002	0.023820258	0.105175599	down_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


beta_glucan_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

beta_glucan_reg

beta_glucan_reg_final <- beta_glucan_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

beta_glucan_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

beta_glucan_reg_final_plot <- beta_glucan_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                                  panel.background = element_rect(fill = "white"), 
                                                                                                  panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                                  axis.title.x = element_text(color="black", vjust=1),
                                                                                                  axis.title.y = element_text(color="black" , vjust=1)) 






beta_glucan_reg_final_plot


jpeg("beta_glucan_regulation_inter.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(beta_glucan_reg_final_plot)
dev.off()


################1,3 beta glucan synthase under


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	under	CvsDr	TRINITY_DN85759_c0_g1	18.98360275	-8.086592809	3.165304557	-2.554759792	0.01062611	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN85759_c0_g1	16.03552908	-7.712882824	3.083260828	-2.501534334	0.012365645	NA	down_regulated
stress	under	CvsDi	TRINITY_DN236596_c0_g1	157.1169518	6.04618856	2.557785607	2.363837119	0.018086763	0.162153655	up_regulated
stress	under	CvsDi	TRINITY_DN47653_c0_g1	12.01409985	6.77322341	3.098469383	2.185990104	0.028816329	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN40509_c0_g1	77.03511704	5.202573716	2.714657294	1.916475324	0.055304621	0.233201153	up_regulated
stress	under	CvsDxD	TRINITY_DN66074_c0_g1	7.757356843	6.182042438	3.103828906	1.99174717	0.046398805	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN12574_c1_g1	15.17621177	7.149955455	3.444026926	2.076045167	0.037889774	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN36266_c0_g1	110.6518732	21.44038983	3.424434225	6.261002087	3.82511E-10	1.61292E-08	up_regulated
stress	under	CvsDi	TRINITY_DN12574_c1_g1	15.17621177	0	3.444026926	2.076045167	0.037889774	NA	down_regulated
stress	under	CvsDr	TRINITY_DN36266_c0_g1	110.6518732	0	3.424434225	6.261002087	3.82511E-10	1.61292E-08	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


beta_glucan_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

beta_glucan_reg

beta_glucan_reg_final <- beta_glucan_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

beta_glucan_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

beta_glucan_reg_final_plot <- beta_glucan_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                                           panel.background = element_rect(fill = "white"), 
                                                                                                           panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                                           axis.title.x = element_text(color="black", vjust=1),
                                                                                                           axis.title.y = element_text(color="black" , vjust=1)) 






beta_glucan_reg_final_plot


jpeg("beta_glucan_regulation_under.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(beta_glucan_reg_final_plot)
dev.off()








################ Calcineurin-like phosphoesterase inter


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
pathogenicity	inter	CvsDi	TRINITY_DN98454_c0_g2	36.47744848	-8.626477067	3.615734566	-2.385815914	0.017041279	0.922241767	down_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN32202_c0_g1	18.89010725	-7.677212401	3.648065482	-2.104461238	0.035338232	0.922241767	down_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN38671_c0_g1	18.38967582	-7.638418306	3.125246656	-2.444100945	0.014521363	0.922241767	down_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN119546_c0_g1	10.67217798	-6.853073656	3.230738545	-2.121209612	0.033904167	0.922241767	down_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN57481_c1_g1	8.071420202	-6.450301354	3.28828688	-1.961599334	0.04980915	0.922241767	down_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN8146_c1_g3	6.666546839	6.337544826	3.352641308	1.890314007	0.058715977	0.922241767	up_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN368727_c0_g1	7.746044454	6.553782838	3.302312389	1.98460414	0.047188531	0.922241767	up_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN529783_c0_g1	7.898609819	6.582189911	3.296479704	1.996733031	0.045854192	0.922241767	up_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN1335698_c0_g1	9.052099007	6.780516805	3.321536031	2.041379874	0.041213083	0.922241767	up_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN590150_c0_g1	9.618406845	6.865899623	3.24212203	2.117717828	0.034198972	0.922241767	up_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN1161203_c0_g1	12.51714879	7.247623521	3.685151608	1.966709729	0.049216685	0.922241767	up_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN10358_c0_g1	13.0580009	7.306601122	3.183370746	2.295240393	0.021719351	0.922241767	up_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN185248_c0_g1	14.10746056	7.418638822	3.158538382	2.348756901	0.0188362	0.922241767	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN30001_c0_g1	35.37603633	-8.476315674	3.712647627	-2.283091887	0.022424956	0.908093125	down_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN2726_c1_g1	20.0870204	-7.660000165	3.722329824	-2.057851004	0.039604438	0.908093125	down_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN98454_c0_g2	35.86875963	-7.010560538	3.487564638	-2.010159313	0.04441433	0.908093125	down_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN15468_c1_g1	10.1296611	-6.672294225	3.265517751	-2.043257681	0.041026937	0.908093125	down_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN24355_c0_g1	7.436540209	6.607690793	3.347607994	1.973854407	0.0483983	0.908093125	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN66000_c0_g1	7.824531157	6.680687601	3.321782477	2.01117552	0.044306921	0.908093125	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN2726_c0_g2	61.19016108	7.21958157	2.877534922	2.508946639	0.012109178	0.908093125	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN49517_c0_g1	17.82949796	7.868692221	3.199775674	2.45913871	0.01392708	0.908093125	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN20889_c0_g1	32.4338388	8.732237912	3.694531426	2.363557622	0.018100412	0.908093125	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN104820_c1_g1	34.03305107	8.801346782	3.223766215	2.730144246	0.006330662	0.908093125	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN44345_c0_g1	64.13120141	9.715504691	3.253033197	2.986598692	0.002820998	0.833604812	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN4475_c0_g3	88.85444789	10.1859604	3.174285645	3.208898486	0.001332445	0.590606367	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN201050_c0_g1	14.606319	7.58080268	3.731483693	2.03157867	0.042196328	0.908093125	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN30001_c0_g1	30.35062769	-8.05582964	3.978467757	-2.02485734	0.042882015	0.935664183	down_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN1285991_c0_g1	7.851305129	6.834279715	3.542022823	1.929484946	0.053670687	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN4475_c0_g3	9.843476532	7.160361764	3.506856821	2.041817539	0.041169634	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN2642_c1_g3	57.44307103	7.216304666	3.224494283	2.237964788	0.025223351	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN153672_c0_g1	12.65335519	7.520111226	3.986402009	1.886440758	0.059235585	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN104025_c0_g1	12.85206629	7.54525539	3.97091866	1.900128418	0.057416269	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN103195_c1_g1	14.05928354	7.672241235	3.979485569	1.927947998	0.053861595	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN550537_c0_g1	15.03965204	7.771837703	3.961386198	1.961898516	0.049774301	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN104820_c1_g2	15.31310026	7.797812895	3.960468822	1.968911572	0.048963246	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN86380_c1_g1	15.4652119	7.809849218	3.97431303	1.965081552	0.049404799	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN103258_c0_g1	16.87114025	7.93546801	3.970410062	1.998652	0.04564602	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN137115_c0_g1	16.87114025	7.93546801	3.970410062	1.998652	0.04564602	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN33837_c0_g3	16.87114025	7.93546801	3.970410062	1.998652	0.04564602	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN36035_c0_g1	19.68299696	8.157999162	3.965216351	2.057390679	0.039648661	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN53847_c0_g1	22.49485367	8.350748841	3.962289829	2.107556287	0.035069388	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN681044_c0_g1	25.30671038	8.520755426	3.96076691	2.151289288	0.031453373	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN924584_c0_g1	29.25895943	8.7313837	3.945978916	2.212729436	0.026916305	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN1335698_c0_g1	32.26688984	8.872502437	3.946220029	2.248354722	0.02455358	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN21079_c0_g2	33.58621276	8.929176499	3.570640775	2.500721036	0.012394076	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN13423_c0_g1	92.79127138	23.08398068	3.984695311	5.7931608	6.91E-09	2.38E-06	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN28695_c0_g1	21.79188949	8.304921446	3.962864288	2.095686565	0.036110004	0.935664183	up_regulated	
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


Calcineurin_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

Calcineurin_reg

Calcineurin_reg_final <- Calcineurin_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

Calcineurin_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

Calcineurin_reg_final_plot <- Calcineurin_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                                           panel.background = element_rect(fill = "white"), 
                                                                                                           panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                                           axis.title.x = element_text(color="black", vjust=1),
                                                                                                           axis.title.y = element_text(color="black" , vjust=1)) 






Calcineurin_reg_final_plot


jpeg("Calcineurin_regulation_inter.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(Calcineurin_reg_final_plot)
dev.off()


################Calcineurin-like phosphoesterase under


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
pathogenicity	under	CvsDi	TRINITY_DN66019_c0_g1	98.30746204	-23.79520984	3.488236208	-6.821559213	9.01E-12	8.59E-09	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN3452_c0_g1	101.6705614	-10.44029468	2.898839406	-3.601542967	0.000316334	0.190622945	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN35261_c9_g1	44.28013163	-9.241477742	3.467839818	-2.664909058	0.007700915	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN104605_c3_g1	13.76165442	-7.555677483	3.031745077	-2.492187598	0.012695896	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN112598_c0_g1	13.6460783	-7.544323177	3.506047946	-2.151802626	0.031412903	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN355870_c0_g1	8.911724605	-6.930302615	3.562961754	-1.945095989	0.051763427	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN19532_c0_g1	8.40914851	-6.845257323	3.121910453	-2.192650118	0.028332597	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN64002_c0_g1	6.955522112	-6.57277621	3.201992792	-2.052714243	0.0401003	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN6863_c1_g1	28.00012472	-5.707242845	2.698542874	-2.114935027	0.034435485	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN20996_c0_g2	11.20226798	-5.685842184	2.871282019	-1.980245112	0.047675993	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN89230_c2_g1	7.571307622	6.052250298	3.204542169	1.88864742	0.058939088	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN101609_c0_g1	8.314532214	6.187100327	3.173387662	1.94968311	0.051213901	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN135423_c0_g1	27.14730058	6.356858122	3.039818956	2.09119629	0.036510472	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN147556_c0_g1	9.491087514	6.37815028	3.220339936	1.980582922	0.047638066	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN29050_c1_g1	10.00202188	6.453709859	3.213948031	2.008031803	0.044639912	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN122061_c2_g1	10.68326769	6.548665332	3.206517498	2.042298331	0.041121948	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN10875_c0_g1	10.76087134	6.558839482	3.126101599	2.098089033	0.035897281	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN122283_c0_g1	14.87907526	7.026104684	3.233057584	2.173207405	0.029764711	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN62789_c0_g2	50.99090412	7.274108334	3.050994987	2.384175774	0.017117424	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN67675_c0_g1	18.22332556	7.3181432	3.507415989	2.086477117	0.036935426	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN105673_c0_g1	21.45924318	7.553808468	3.499619916	2.15846539	0.030891666	0.979901061	up_regulated
pathogenicity	under	CvsDr	TRINITY_DN66019_c0_g1	113.060051	-23.7903463	3.719613141	-6.395919522	1.60E-10	4.65E-07	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN325546_c0_g1	17.28776598	-7.93820016	3.339070409	-2.377368305	0.017436668	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN70643_c2_g1	16.61327245	-7.880813019	3.224583615	-2.443978498	0.014526292	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN313471_c0_g1	14.04723538	-7.638769362	3.23070297	-2.364429486	0.018057865	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN35261_c3_g1	13.43305593	-7.574238586	3.286617695	-2.304569405	0.021190701	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN20608_c0_g1	9.276910593	-7.040139797	3.355799737	-2.097902243	0.035913782	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN105676_c0_g1	8.632574102	-6.936292471	3.345907052	-2.073067889	0.038165962	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN34153_c0_g1	55.78338475	-6.903807205	2.843808665	-2.427662343	0.015196484	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN167609_c0_g1	7.679263071	-6.767441498	3.411224545	-1.983874532	0.047269829	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN36911_c0_g1	7.65287566	-6.762566835	3.322584644	-2.035333199	0.041817358	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN138058_c0_g1	7.362749271	-6.706703487	3.469517418	-1.933036408	0.053231713	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN1263207_c0_g1	6.710873519	-6.572995897	3.38778437	-1.940204919	0.052354791	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN90005_c2_g1	6.669407587	-6.564242703	3.366258916	-1.950011234	0.05117478	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN31408_c0_g1	6.398129349	-6.504110231	3.44304477	-1.889057699	0.058884098	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN326873_c0_g2	6.390590089	-6.502443817	3.397719181	-1.913767287	0.055649897	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN77552_c0_g1	6.356663417	-6.494933638	3.370546928	-1.926967277	0.053983708	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN162086_c0_g1	6.066537028	-6.427387572	3.396993225	-1.892081363	0.058480141	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN36911_c3_g3	16.54613319	-6.245698807	3.030371893	-2.06103377	0.039299818	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN39450_c0_g1	10.03908377	-5.766427317	3.048645101	-1.891472154	0.058561345	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN122061_c1_g2	29.71891893	-5.535232308	2.852900004	-1.94021252	0.052353868	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN48752_c0_g1	26.40747943	-4.892818987	2.559781748	-1.911420375	0.05595058	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN144986_c0_g1	21.55039673	7.524406721	3.711745363	2.027188286	0.042643161	0.962938997	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1416171_c0_g1	9.643918779	-7.036424901	3.146573702	-2.236218048	0.025337492	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1354525_c0_g1	8.309339412	-6.8230494	3.316997751	-2.056995486	0.03968666	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1557454_c0_g1	6.085984662	-6.372651508	3.249146782	-1.961330754	0.049840452	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1263207_c0_g1	5.687972097	-6.275907161	3.286107407	-1.909830198	0.05615508	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN313471_c0_g1	12.33647858	-5.992554864	2.907944889	-2.060752557	0.039326653	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN11751_c3_g1	9.715111261	-5.641913778	2.931311213	-1.924706511	0.054266085	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN24346_c0_g1	51.61033726	5.593099691	2.696351205	2.074321654	0.038049448	0.173542605	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN162455_c0_g1	8.193318648	6.262360497	3.187606548	1.964596447	0.049460964	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN166549_c0_g1	8.215303401	6.263094676	3.171076885	1.975068692	0.048260353	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN690171_c0_g1	24.51060172	6.28643624	3.33929329	1.88256487	0.059759361	0.253977284	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN947496_c0_g1	12.33819387	6.852840741	3.594555912	1.906449895	0.05659186	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN468039_c0_g1	12.86553195	6.909810627	3.614645518	1.91161501	0.055925593	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1312601_c0_g1	14.55080438	7.088144507	3.085194742	2.297470695	0.021591933	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN983507_c0_g1	14.64132339	7.099492764	3.58083155	1.982638017	0.047407878	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1551617_c0_g1	14.83585338	7.118276684	3.098986034	2.296969591	0.021620505	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN130680_c0_g1	16.77994367	7.296000424	3.572560275	2.042232982	0.041128426	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1008324_c0_g1	19.25940801	7.493620103	3.038051189	2.466587835	0.013640727	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN38753_c0_g1	21.35572517	7.641749094	3.061195117	2.496328657	0.012548628	0.135088704	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN122061_c3_g2	21.87169448	7.678273443	3.219094615	2.385227637	0.017068556	0.135088704	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN81237_c1_g3	22.44345887	7.715475507	3.188865447	2.419504878	0.015541652	0.135088704	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN116547_c0_g3	23.03129523	7.752507547	3.560817849	2.177170491	0.029467849	0.145012836	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN216128_c0_g1	78.00096922	7.988688114	2.805253596	2.847759692	0.004402816	0.077935997	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN95525_c0_g1	35.20497985	8.364354092	3.557430696	2.351234587	0.018711233	0.135088704	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1587001_c0_g1	37.33556413	8.449252492	3.222246703	2.622161886	0.008737392	0.10892615	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN4387_c2_g1	38.73533076	8.501362205	2.998857049	2.834867439	0.00458447	0.077935997	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN956653_c0_g1	16.61543441	7.281799236	3.573086141	2.03795793	0.04155414	NA	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


Calcineurin_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

Calcineurin_reg

Calcineurin_reg_final <- Calcineurin_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

Calcineurin_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

Calcineurin_reg_final_plot <- Calcineurin_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                                           panel.background = element_rect(fill = "white"), 
                                                                                                           panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                                           axis.title.x = element_text(color="black", vjust=1),
                                                                                                           axis.title.y = element_text(color="black" , vjust=1)) 






Calcineurin_reg_final_plot


jpeg("Calcineurin_regulation_under.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(Calcineurin_reg_final_plot)
dev.off()







################ CLP amino terminal inter


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
pathogenicity	inter	CvsDi	TRINITY_DN929082_c0_g1	55.04572622	-9.219915643	3.617355647	-2.548799881	0.010809431	0.922241767	down_regulated
pathogenicity	inter	CvsDi	TRINITY_DN30866_c0_g1	23.1995726	-7.973683229	3.149815359	-2.531476395	0.011358346	0.922241767	down_regulated
pathogenicity	inter	CvsDi	TRINITY_DN26131_c0_g1	20.35304163	-7.784438141	3.645735213	-2.135217641	0.032743237	0.922241767	down_regulated
pathogenicity	inter	CvsDi	TRINITY_DN1325222_c0_g1	14.33042619	-7.278726834	3.678018321	-1.978980581	0.047818193	0.922241767	down_regulated
pathogenicity	inter	CvsDi	TRINITY_DN24844_c2_g1	7.563441139	6.520629102	3.323219388	1.96214223	0.049745927	0.922241767	up_regulated
pathogenicity	inter	CvsDi	TRINITY_DN83095_c0_g2	8.25923423	6.648640399	3.389794924	1.961369507	0.049835934	0.922241767	up_regulated
pathogenicity	inter	CvsDi	TRINITY_DN7990_c0_g1	9.137630636	6.794305137	3.368005835	2.017308007	0.043663381	0.922241767	up_regulated
pathogenicity	inter	CvsDi	TRINITY_DN241587_c0_g1	11.41915328	7.115291864	3.700591234	1.922744614	0.054512131	0.922241767	up_regulated
pathogenicity	inter	CvsDi	TRINITY_DN52430_c0_g1	22.04764839	8.062399764	3.113792116	2.589254345	0.009618402	0.922241767	up_regulated
pathogenicity	inter	CvsDi	TRINITY_DN27643_c1_g1	66.94073019	9.665566952	3.189722345	3.03022204	0.00244374	0.806434231	up_regulated
pathogenicity	inter	CvsDi	TRINITY_DN68002_c2_g1	111.5563436	23.09517202	3.615931293	6.387060525	1.69E-10	9.30E-08	up_regulated
pathogenicity	inter	CvsDr	TRINITY_DN929082_c0_g1	48.29000865	-8.925371406	3.712102415	-2.404397942	0.016199129	0.908093125	down_regulated
pathogenicity	inter	CvsDr	TRINITY_DN26635_c0_g1	6.505757565	6.414276102	3.37106402	1.902745265	0.057073792	0.908093125	up_regulated
pathogenicity	inter	CvsDr	TRINITY_DN15798_c0_g1	15.1681005	7.635262394	3.728418214	2.047855674	0.040574143	0.908093125	up_regulated
pathogenicity	inter	CvsDr	TRINITY_DN4731_c0_g1	15.19195078	7.637621493	3.237352508	2.359218366	0.018313476	0.908093125	up_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN929082_c0_g1	45.88547158	-8.651994365	3.986245599	-2.170461942	0.029971869	0.935664183	down_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN30866_c0_g1	19.40819585	-7.410870215	3.464643241	-2.138999516	0.032435708	0.935664183	down_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN8760_c0_g1	16.71311361	6.440702745	3.253271194	1.979762018	0.047730276	0.935664183	up_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN36847_c0_g1	8.905092045	7.013996916	3.506176119	2.00046908	0.045449635	0.935664183	up_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN265795_c0_g1	10.03823153	7.187678745	3.482597652	2.063884337	0.039028684	0.935664183	up_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN24844_c2_g1	13.74780742	7.640335857	3.460877426	2.207629718	0.027270096	0.935664183	up_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN43276_c0_g1	17.22262234	7.965235349	3.969592393	2.006562529	0.044796265	0.935664183	up_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN88021_c0_g1	23.90078202	8.43825483	3.961389965	2.130124755	0.033161315	0.935664183	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


clp_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

clp_reg

clp_reg_final <- clp_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

clp_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

clp_reg_final_plot <- clp_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                                           panel.background = element_rect(fill = "white"), 
                                                                                                           panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                                           axis.title.x = element_text(color="black", vjust=1),
                                                                                                           axis.title.y = element_text(color="black" , vjust=1)) 






clp_reg_final_plot


jpeg("clpaminoterminal_regulation_inter.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(clp_reg_final_plot)
dev.off()





################ CLP amino terminal under


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
pathogenicity	under	CvsDi	TRINITY_DN780055_c0_g2	14.2033659	-7.599220825	3.516117741	-2.16125323	0.030675784	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN280474_c1_g1	11.97512994	-7.356066597	3.519985421	-2.089800302	0.036635742	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN1175922_c0_g1	11.77840099	-7.328871734	3.535783445	-2.072771664	0.038193535	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN53697_c1_g1	6.941996641	-6.567118476	3.16791166	-2.07301187	0.038171175	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN64340_c0_g1	26.88099128	-5.87989635	2.701567764	-2.176475611	0.029519715	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN37175_c0_g1	8.407760746	6.202153624	3.120665467	1.987445848	0.046873011	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN79300_c1_g1	9.150464606	6.325484598	3.224504469	1.961691993	0.049798355	0.979901061	up_regulated
pathogenicity	under	CvsDr	TRINITY_DN280474_c1_g1	13.77218751	-7.61018164	3.721434699	-2.04495907	0.040858893	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN780055_c0_g2	12.97706581	-7.524532652	3.725755768	-2.019599008	0.043424998	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN1175922_c0_g1	10.76146921	-7.254461454	3.745069639	-1.937069842	0.052736807	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN1669707_c0_g1	9.273140963	-7.039562251	3.336461538	-2.109888626	0.03486795	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN394943_c0_g1	6.680716478	-6.566635889	3.351762716	-1.959158939	0.050094176	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN32304_c1_g1	6.398129349	-6.504110231	3.44304477	-1.889057699	0.058884098	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN6403_c0_g2	24.91396401	7.731472525	3.280551647	2.356759886	0.018435163	0.962938997	up_regulated
pathogenicity	under	CvsDr	TRINITY_DN467012_c0_g1	26.84371575	7.8393521	3.734154368	2.099364763	0.035784758	0.962938997	up_regulated
pathogenicity	under	CvsDr	TRINITY_DN4210_c0_g1	102.3710007	8.329843541	3.54067187	2.352616635	0.018641842	0.962938997	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN780055_c0_g2	13.43741089	-7.512422757	3.585364027	-2.095302653	0.036144097	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1175922_c0_g1	11.14321879	-7.242059594	3.603237124	-2.009875938	0.044444321	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN280474_c1_g1	11.0712484	-7.236196521	3.588900025	-2.016271412	0.043771603	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1891971_c0_g1	9.268952148	-6.980166876	3.611382643	-1.932823953	0.053257889	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1384041_c0_g1	56.38291531	5.139801973	2.548273533	2.016974201	0.043698206	0.194561059	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN4210_c0_g1	37.96821642	6.938841007	2.944688847	2.35639192	0.018453437	0.135088704	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN6403_c0_g2	18.71554116	7.45085989	3.138969499	2.373664316	0.017612554	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN999723_c0_g1	20.87614618	7.608765212	3.587019083	2.121194517	0.033905436	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN40606_c0_g1	21.31554525	7.63872848	3.11163968	2.454888504	0.014092829	0.135088704	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN794616_c0_g1	39.32483351	8.522825862	3.5813036	2.379811045	0.017321518	0.135088704	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN7519_c0_g1	120.1592135	22.85697927	3.61228638	6.327565663	2.49E-10	1.32E-08	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1823270_c0_g1	129.626303	22.92259682	3.615356437	6.340342154	2.29E-10	1.32E-08	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


clp_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

clp_reg

clp_reg_final <- clp_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

clp_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

clp_reg_final_plot <- clp_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                           panel.background = element_rect(fill = "white"), 
                                                                                           panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                           axis.title.x = element_text(color="black", vjust=1),
                                                                                           axis.title.y = element_text(color="black" , vjust=1)) 






clp_reg_final_plot


jpeg("clpaminoterminal_regulation_under.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(clp_reg_final_plot)
dev.off()







################ heat shock protein 9/12 inter


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	inter	CvsDi	TRINITY_DN16682_c0_g1	376.7064098	-25.45947281	3.547096753	-7.1775524	7.10E-13	8.14E-11	down_regulated
stress	inter	CvsDi	TRINITY_DN3381_c0_g1	51.0061871	-9.282782652	3.05121737	-3.04232099	0.002347614	0.04250417	down_regulated
stress	inter	CvsDi	TRINITY_DN5386_c2_g1	50.96900271	-9.28171139	2.981297497	-3.11331271	0.001849999	0.035355534	down_regulated
stress	inter	CvsDi	TRINITY_DN81460_c0_g1	36.36347094	-7.360326152	3.291361675	-2.236255653	0.02533503	0.202680236	down_regulated
stress	inter	CvsDi	TRINITY_DN27148_c0_g1	12.7520442	-7.282072704	3.138583637	-2.320177999	0.02033125	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN236523_c0_g1	11.38353461	-7.119662824	3.310177794	-2.150840005	0.03148883	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN26649_c0_g1	166.1415633	-6.031810915	2.851591225	-2.11524389	0.034409166	0.246599023	down_regulated
stress	inter	CvsDi	TRINITY_DN1162041_c0_g1	11.66086137	7.011699	3.621378282	1.936196237	0.052843671	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN23708_c0_g1	142.9621604	23.30854619	3.512128767	6.63658645	3.21E-11	1.00E-09	up_regulated
stress	inter	CvsDr	TRINITY_DN16682_c0_g1	320.065317	-24.93120513	3.633918878	-6.860693912	6.85E-12	5.39E-10	down_regulated
stress	inter	CvsDr	TRINITY_DN26649_c0_g1	139.0338981	-23.82152017	3.598694261	-6.619489861	3.60E-11	1.46E-09	down_regulated
stress	inter	CvsDr	TRINITY_DN29285_c0_g1	88.19479384	-23.20637161	3.583997941	-6.4749958	9.48E-11	2.98E-09	down_regulated
stress	inter	CvsDr	TRINITY_DN5386_c3_g1	36.69257087	-8.516274828	3.576081078	-2.381454627	0.017244415	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN81460_c0_g1	34.84171028	-8.441567412	3.57754767	-2.359596067	0.018294843	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN3403_c0_g1	13.54957969	7.372358378	3.157228655	2.33507268	0.019539624	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN2097_c0_g2	43.81969412	9.065986734	3.566843599	2.541739351	0.01103024	0.09027918	up_regulated
stress	inter	CvsDxD	TRINITY_DN16682_c0_g1	319.9754741	-24.74932824	3.962666286	-6.245625157	4.22E-10	2.85E-08	down_regulated
stress	inter	CvsDxD	TRINITY_DN29285_c0_g1	88.1700374	-22.99189894	3.911967375	-5.877323794	4.17E-09	5.57E-08	down_regulated
stress	inter	CvsDxD	TRINITY_DN5386_c3_g1	36.6822712	-8.293787909	3.897016697	-2.128240281	0.033317167	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN81460_c0_g1	29.44290764	-7.977085436	3.88979504	-2.05077269	0.040289088	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN26649_c0_g1	140.2308019	-6.809810775	3.539035683	-1.924199524	0.054329579	0.202501157	down_regulated
stress	inter	CvsDxD	TRINITY_DN5386_c2_g1	42.38277967	-6.005706001	3.01554624	-1.991581466	0.046416999	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN76258_c0_g2	15.02262004	7.643143963	3.901346816	1.959103952	0.050100614	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1385446_c0_g1	22.53393006	8.228185597	3.878335919	2.121576307	0.033873334	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1367435_c0_g1	40.82233707	9.085513452	3.870322517	2.347482261	0.018900773	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN781459_c0_g1	50.36418487	9.388729913	3.869230517	2.426510872	0.015244794	0.082986683	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


HSP912_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

HSP912_reg

HSP912_reg_final <- HSP912_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

HSP912_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

HSP912_reg_final_plot <- HSP912_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                           panel.background = element_rect(fill = "white"), 
                                                                                           panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                           axis.title.x = element_text(color="black", vjust=1),
                                                                                           axis.title.y = element_text(color="black" , vjust=1)) 






HSP912_reg_final_plot


jpeg("HSP912_regulation_inter.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(HSP912_reg_final_plot)
dev.off()




################ heat shock protein 9/12 under


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	under	CvsDi	TRINITY_DN448268_c0_g1	35.44327047	-8.885368154	2.988445563	-2.973240759	0.002946732	0.073517189	down_regulated
stress	under	CvsDi	TRINITY_DN730953_c0_g1	161.8104893	-7.649079337	2.906641318	-2.631586942	0.008498712	0.117778564	down_regulated
stress	under	CvsDi	TRINITY_DN1249926_c0_g1	10.29408531	-7.103079678	3.370480105	-2.107438542	0.035079584	NA	down_regulated
stress	under	CvsDi	TRINITY_DN109666_c1_g1	7.431600851	-6.627747144	3.478476679	-1.905359086	0.05673341	NA	down_regulated
stress	under	CvsDi	TRINITY_DN10633_c1_g2	19.34817783	5.904652457	3.110438761	1.898334258	0.057652064	0.315143023	up_regulated
stress	under	CvsDi	TRINITY_DN118_c2_g1	23.46498011	7.738632717	3.28536622	2.355485568	0.018498516	0.162153655	up_regulated
stress	under	CvsDi	TRINITY_DN1756643_c0_g1	50.6276377	8.848201479	3.257922663	2.71590286	0.006609531	0.107184558	up_regulated
stress	under	CvsDr	TRINITY_DN15593_c0_g1	72.1934591	-10.01332341	3.139716086	-3.189244867	0.00142645	0.033912697	down_regulated
stress	under	CvsDr	TRINITY_DN448268_c0_g1	40.28974388	-9.171974862	3.207213443	-2.85979559	0.004239142	0.072656914	down_regulated
stress	under	CvsDr	TRINITY_DN1249926_c0_g1	11.71824154	-7.390731985	3.55656	-2.07805632	0.037704172	NA	down_regulated
stress	under	CvsDr	TRINITY_DN10469_c0_g1	6.225897209	-6.477952995	3.243481869	-1.997221892	0.045801084	NA	down_regulated
stress	under	CvsDr	TRINITY_DN52451_c0_g1	15.15477861	-5.552727495	2.814064032	-1.973205809	0.048472119	NA	down_regulated
stress	under	CvsDr	TRINITY_DN2704_c0_g1	28.98972305	-4.490587594	2.364785753	-1.898940565	0.057572291	0.263545208	down_regulated
stress	under	CvsDr	TRINITY_DN2140_c0_g1	257.4527138	5.72539005	2.90505112	1.97083969	0.048742215	0.239003112	up_regulated
stress	under	CvsDr	TRINITY_DN118_c0_g1	134.9949779	6.123898612	2.45873361	2.49067186	0.012750182	0.113215468	up_regulated
stress	under	CvsDr	TRINITY_DN291582_c0_g1	18.68080055	7.340040483	3.524418805	2.082624367	0.037285476	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1040281_c0_g1	24.75947375	7.74611211	3.509531417	2.207164202	0.02730259	0.164934497	up_regulated
stress	under	CvsDr	TRINITY_DN93229_c0_g1	112.6853998	8.45650911	2.972841878	2.844587589	0.004446896	0.072830271	up_regulated
stress	under	CvsDr	TRINITY_DN1849316_c0_g1	51.29807136	8.796446973	3.501966728	2.511859094	0.012009703	0.107940868	up_regulated
stress	under	CvsDr	TRINITY_DN3292_c0_g1	133.2684415	22.890394	3.55626539	6.436638297	1.22E-10	8.34E-09	up_regulated
stress	under	CvsDxD	TRINITY_DN730953_c0_g1	153.6505595	-24.31082103	3.414147918	-7.12061153	1.07E-12	1.17E-10	down_regulated
stress	under	CvsDxD	TRINITY_DN448268_c0_g1	33.83609626	-8.789192257	3.115740374	-2.820900076	0.004788912	0.059571706	down_regulated
stress	under	CvsDxD	TRINITY_DN6446_c0_g1	15.08564537	-7.621154227	3.439100036	-2.216031563	0.026689341	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN68171_c0_g1	16.73611391	-5.205193076	2.705118083	-1.924201797	0.054329294	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN15593_c0_g1	62.50486486	-5.0818996	2.698028867	-1.883560129	0.059624499	0.240718058	down_regulated
stress	under	CvsDxD	TRINITY_DN10469_c0_g1	153.6053338	4.695824525	2.462429414	1.90698848	0.056522079	0.234081154	up_regulated
stress	under	CvsDxD	TRINITY_DN121361_c0_g1	7.851175578	6.197073711	3.26190732	1.899831326	0.057455258	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN13962_c0_g1	55.51906216	6.554794759	2.705989631	2.422328114	0.015421419	0.099995162	up_regulated
stress	under	CvsDxD	TRINITY_DN34028_c0_g1	37.50426066	6.891682177	3.213676417	2.144485406	0.031994014	0.153692761	up_regulated
stress	under	CvsDxD	TRINITY_DN16698_c0_g1	29.50995246	8.108315837	3.416570441	2.37323245	0.017633163	0.109701398	up_regulated
stress	under	CvsDxD	TRINITY_DN1859834_c0_g1	33.33948828	8.284384122	3.414370433	2.42632845	0.015252459	0.099995162	up_regulated
stress	under	CvsDxD	TRINITY_DN26368_c0_g1	37.16902409	8.441283054	3.413307341	2.473050977	0.013396503	0.094401759	up_regulated
stress	under	CvsDxD	TRINITY_DN29692_c0_g1	39.91793183	8.544215704	2.936096939	2.91005913	0.003613604	0.048977238	up_regulated
stress	under	CvsDxD	TRINITY_DN108196_c0_g1	47.04760099	8.781329474	3.154370618	2.783861041	0.005371604	0.063141349	up_regulated
stress	under	CvsDxD	TRINITY_DN3132_c3_g1	96.63946265	22.5633403	3.429125197	6.579911494	4.71E-11	2.13E-09	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


HSP912_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

HSP912_reg

HSP912_reg_final <- HSP912_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

HSP912_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

HSP912_reg_final_plot <- HSP912_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                                 panel.background = element_rect(fill = "white"), 
                                                                                                 panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                                 axis.title.x = element_text(color="black", vjust=1),
                                                                                                 axis.title.y = element_text(color="black" , vjust=1)) 






HSP912_reg_final_plot


jpeg("HSP912_regulation_under.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(HSP912_reg_final_plot)
dev.off()




################ heat shock protein 20 alpha crystallin family inter


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	inter	CvsDi	TRINITY_DN725520_c0_g1	497.3077027	-25.83959131	3.553406011	-7.271781279	3.55E-13	6.10E-11	down_regulated
stress	inter	CvsDi	TRINITY_DN143_c0_g2	219.9184575	-24.72184942	3.516899072	-7.029445233	2.07E-12	1.78E-10	down_regulated
stress	inter	CvsDi	TRINITY_DN31886_c0_g1	126.5284276	-23.96637171	3.496553524	-6.854284239	7.17E-12	4.93E-10	down_regulated
stress	inter	CvsDi	TRINITY_DN363_c4_g1	82.46956624	-23.38338876	3.49252917	-6.695259401	2.15E-11	8.23E-10	down_regulated
stress	inter	CvsDi	TRINITY_DN1351665_c0_g1	78.30706447	-23.30056704	3.491676706	-6.673174238	2.50E-11	8.61E-10	down_regulated
stress	inter	CvsDi	TRINITY_DN1174_c0_g2	57.42470378	-9.453524779	2.907833537	-3.251054319	0.001149779	0.023266114	down_regulated
stress	inter	CvsDi	TRINITY_DN5729_c0_g1	54.77520102	-9.385611421	3.131380108	-2.997276312	0.002724037	0.046853434	down_regulated
stress	inter	CvsDi	TRINITY_DN34378_c0_g1	52.55158479	-9.325437155	3.489223794	-2.672639448	0.007525709	0.089270477	down_regulated
stress	inter	CvsDi	TRINITY_DN38017_c0_g1	48.43304281	-9.208032987	3.482845579	-2.643824648	0.008197511	0.09096593	down_regulated
stress	inter	CvsDi	TRINITY_DN2160_c0_g1	244.1914929	-9.151058736	3.217422044	-2.844220811	0.004452018	0.0729283	down_regulated
stress	inter	CvsDi	TRINITY_DN55012_c0_g1	42.40787959	-9.016401409	3.484711224	-2.5874171	0.009669846	0.097685577	down_regulated
stress	inter	CvsDi	TRINITY_DN31824_c1_g1	36.94220317	-8.816914139	3.495097378	-2.522651928	0.011647362	0.10636438	down_regulated
stress	inter	CvsDi	TRINITY_DN690442_c0_g1	29.08067373	-8.471608276	3.173856624	-2.669184302	0.007603572	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN35861_c0_g2	24.9750106	-8.252033297	3.514578667	-2.34794383	0.018877368	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1460036_c0_g1	24.45469787	-8.221653358	3.516118673	-2.338275275	0.019372973	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN347341_c0_g1	19.25157067	-7.876437266	3.537948058	-2.226272725	0.025995919	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN6830_c0_g1	19.1969905	-7.873163868	3.046939407	-2.583958135	0.009767365	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN122091_c0_g1	17.38027852	-7.729805802	3.542852633	-2.181802802	0.02912409	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN492544_c0_g1	17.03048168	-7.700592903	3.148985687	-2.445420103	0.01446835	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN10353_c0_g1	16.91016342	-7.689300503	3.553539797	-2.163842518	0.030476438	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN26986_c0_g1	16.51229304	-7.655036706	3.074270688	-2.490033404	0.012773109	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN19398_c0_g1	16.3898507	-7.64419928	3.557743289	-2.148609009	0.031665407	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1346004_c0_g1	15.60938162	-7.573788584	3.564674295	-2.124678991	0.033613408	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN16579_c0_g2	15.49358029	-7.564136509	3.120626893	-2.423915697	0.015354169	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN53805_c0_g1	15.34922526	-7.549533487	3.567169017	-2.116393546	0.03431135	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN45156_c0_g2	15.06290805	-7.5234216	3.562880173	-2.111612301	0.034719717	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN2999_c0_g1	13.3619974	-7.349653313	3.100465053	-2.370500291	0.01776403	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN16109_c0_g2	13.1630622	-7.328223572	3.086335837	-2.374408995	0.017577069	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN3363_c1_g2	12.4875053	-7.251751739	3.602650542	-2.012893467	0.044125838	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN626_c0_g1	12.27329841	-7.227740342	3.107403983	-2.32597383	0.020019947	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1345002_c0_g1	12.142091	-7.211187653	3.222611227	-2.237684643	0.025241627	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN23371_c0_g1	11.22829043	-7.099135153	3.121011873	-2.274626128	0.022928367	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN221_c0_g2	855.3545927	-7.055955579	2.904331508	-2.429459433	0.015121357	0.126871874	down_regulated
stress	inter	CvsDi	TRINITY_DN39775_c0_g1	10.65990416	-7.0248065	3.629538088	-1.935454686	0.052934524	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN784030_c0_g1	10.40625441	-6.988614206	3.642420386	-1.918673153	0.055025711	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN184502_c0_g1	10.25326869	-6.968859949	3.27604526	-2.127217238	0.033402038	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN38674_c0_g1	10.14609805	-6.952072957	3.648644748	-1.905384995	0.056730044	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN84137_c0_g1	10.14609805	-6.952072957	3.648644748	-1.905384995	0.056730044	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1223037_c0_g1	9.964693018	-6.927551664	3.64607466	-1.900002691	0.057432767	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN17808_c0_g1	55.15637753	-6.885820002	3.162684452	-2.177207403	0.029465096	0.22278813	down_regulated
stress	inter	CvsDi	TRINITY_DN13238_c0_g1	23.36640768	-6.716942528	3.315675686	-2.025814092	0.042783839	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN2999_c0_g2	7.922750604	-6.595124766	3.282152614	-2.009390038	0.044495786	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN50525_c0_g2	7.63417493	-6.541655822	3.281608444	-1.993429726	0.046214412	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN801383_c0_g1	7.057023583	-6.428405247	3.290637656	-1.953543939	0.050755176	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN593_c0_g2	90.7485589	-6.341938763	2.442910696	-2.596058371	0.009430004	0.097685577	down_regulated
stress	inter	CvsDi	TRINITY_DN112586_c0_g1	15.64153958	-6.083061281	2.984905157	-2.037941228	0.041555811	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN14273_c0_g1	47.02288622	-5.427182101	2.633105462	-2.061133585	0.039290297	0.275833925	down_regulated
stress	inter	CvsDi	TRINITY_DN21868_c0_g1	21.09623177	-4.503203472	2.391883869	-1.882701552	0.059740825	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN3206_c0_g1	51.46093204	-4.315700772	2.222024863	-1.942237841	0.052108314	0.358505201	down_regulated
stress	inter	CvsDi	TRINITY_DN626_c2_g1	332.5820884	4.041887954	2.139696146	1.889000904	0.058891708	0.385014649	up_regulated
stress	inter	CvsDi	TRINITY_DN7376_c0_g1	74.34133983	4.952630085	2.576941081	1.921902725	0.054617999	0.368403758	up_regulated
stress	inter	CvsDi	TRINITY_DN112275_c0_g1	31.56943358	4.995464371	2.369200206	2.108502421	0.034987554	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN15751_c0_g1	6.664906977	6.205212332	3.29192615	1.884979204	0.059432646	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN29121_c0_g1	6.664906977	6.205212332	3.29192615	1.884979204	0.059432646	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN1191980_c0_g1	6.753044897	6.224606205	3.304538209	1.883653876	0.059611809	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN705856_c0_g1	7.343833043	6.345123783	3.253650088	1.950155552	0.051157582	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN912320_c0_g1	21.24970682	6.373225194	3.324088069	1.917285301	0.055201696	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN8287_c3_g1	8.199034952	6.50463967	3.259566582	1.995553552	0.04598254	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN793628_c0_g1	8.240486182	6.511821955	3.241007781	2.009196643	0.044516284	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN20368_c0_g1	8.452977794	6.548552484	3.233888154	2.024978036	0.04286962	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN886150_c0_g1	8.940137864	6.629263865	3.201806957	2.070475814	0.038407809	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN981588_c0_g1	9.028275785	6.643654809	3.246193607	2.046598452	0.040697527	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN11388_c0_g1	26.03114381	6.73410199	2.817300028	2.39026796	0.016836084	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN567360_c0_g1	9.670769857	6.741667412	3.223653289	2.091312808	0.036500032	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN56646_c0_g2	10.4430693	6.853094222	3.139159512	2.183098436	0.029028562	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN914897_c0_g1	10.64530622	6.881359955	3.329167525	2.066991193	0.038734984	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN73955_c0_g1	10.96120969	6.922405751	3.634953535	1.904400066	0.056858101	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN182419_c0_g1	11.26205544	6.962475524	3.623175829	1.921649915	0.054649823	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN131728_c0_g1	11.51599828	6.99474443	3.26098596	2.144978395	0.031954574	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN391824_c0_g1	11.91502043	7.043695967	3.137066607	2.245312851	0.024748056	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN33856_c0_g1	12.12729582	7.068298152	3.613277177	1.956201478	0.050441425	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN94523_c0_g1	12.17921797	7.074753284	3.107226906	2.276870502	0.02279396	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN1337343_c0_g1	13.81195479	7.256840617	3.58330341	2.025181735	0.042848706	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN520918_c0_g1	13.97230803	7.272601067	3.290462044	2.210206643	0.027090824	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN43163_c0_g1	14.1021134	7.286759241	3.094721046	2.35457708	0.018543797	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN121998_c0_g1	14.4594681	7.322120389	3.581389435	2.044491536	0.040905013	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN67061_c0_g2	79.5103538	7.397899737	3.177479408	2.32822901	0.019899947	0.162990038	up_regulated
stress	inter	CvsDi	TRINITY_DN551_c0_g1	18.94251756	7.711908531	3.04242927	2.534786464	0.011251589	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN1089_c0_g1	19.87538647	7.781264635	3.03782116	2.561462385	0.010423253	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN181468_c0_g1	20.75633323	7.843765675	3.535179592	2.218774314	0.026502082	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN2512_c4_g1	20.76178492	7.844526819	2.999716952	2.615089005	0.008920421	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN55029_c0_g1	21.54455528	7.898187326	3.13679688	2.517914812	0.011805187	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN892195_c0_g1	24.7210261	8.095994084	3.520356303	2.299765532	0.021461507	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN14319_c0_g1	79.25937132	22.50075586	3.491405347	6.444612879	1.16E-10	3.32E-09	up_regulated
stress	inter	CvsDi	TRINITY_DN3901_c0_g2	199.8671638	23.77579815	3.524446288	6.745966943	1.52E-11	7.27E-10	up_regulated
stress	inter	CvsDr	TRINITY_DN725520_c0_g1	479.2968606	-25.51038043	3.653535957	-6.982381104	2.90E-12	4.11E-10	down_regulated
stress	inter	CvsDr	TRINITY_DN2160_c0_g1	207.1140898	-24.36699114	3.614579067	-6.741308101	1.57E-11	7.42E-10	down_regulated
stress	inter	CvsDr	TRINITY_DN221_c0_g2	722.5096849	-9.451480431	2.93965068	-3.215171278	0.001303667	0.024595853	down_regulated
stress	inter	CvsDr	TRINITY_DN1351665_c0_g1	66.53291465	-9.374831668	3.577800017	-2.620278278	0.008785804	0.09027918	down_regulated
stress	inter	CvsDr	TRINITY_DN17808_c0_g1	52.70925401	-9.038803838	3.575559855	-2.527940855	0.011473368	0.09027918	down_regulated
stress	inter	CvsDr	TRINITY_DN38017_c0_g1	46.678958	-8.863521729	3.575093871	-2.479241678	0.013166206	0.096583235	down_regulated
stress	inter	CvsDr	TRINITY_DN9736_c0_g1	46.23226941	-8.849649541	3.575090681	-2.475363657	0.013310057	0.096583235	down_regulated
stress	inter	CvsDr	TRINITY_DN18826_c0_g1	36.62846465	-8.513716848	3.576723714	-2.380311573	0.017298005	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN31824_c1_g1	31.38762086	-8.290989271	3.579260641	-2.316397184	0.020536592	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN35861_c0_g2	21.21980002	-7.726223043	3.596889494	-2.148029027	0.03171145	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN44472_c0_g1	18.98426522	-7.565555154	3.605651791	-2.098248969	0.035883158	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN284222_c0_g1	17.02004793	-7.408058667	3.614628686	-2.04946602	0.040416567	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN347341_c0_g1	16.35692918	-7.350727585	3.618556945	-2.031397515	0.042214686	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN684852_c0_g1	15.61566306	-7.283769245	3.171515895	-2.29662076	0.021640413	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN45156_c0_g2	14.51737928	-7.178528106	3.632490068	-1.97620034	0.048132091	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN26986_c0_g1	14.39983891	-7.166909046	3.142518825	-2.280625653	0.022570608	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN20083_c0_g1	14.36757293	-7.163649484	3.633128622	-1.971757741	0.048637269	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN19398_c0_g1	13.92549376	-7.118563309	3.63706729	-1.957226178	0.050320882	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN1346004_c0_g1	13.26237501	-7.048176871	3.643569373	-1.934415445	0.053062069	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN11490_c0_g1	12.50728061	-6.963511397	3.652606408	-1.906449975	0.05659185	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN32288_c0_g1	33.70660478	-6.961156511	2.900116106	-2.400302697	0.016381519	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN184502_c0_g1	9.822539627	-6.61491577	3.326140855	-1.988765978	0.04672704	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN332_c0_g1	155.8337816	4.338049753	2.292941718	1.89191453	0.05850237	0.312380577	up_regulated
stress	inter	CvsDr	TRINITY_DN8060_c0_g1	83.4530869	4.490388121	2.356210653	1.905766836	0.056680463	0.308472522	up_regulated
stress	inter	CvsDr	TRINITY_DN2607_c0_g1	210.6417678	4.508927672	2.320416983	1.943154056	0.051997547	0.293673665	up_regulated
stress	inter	CvsDr	TRINITY_DN14273_c0_g1	1403.317543	4.958341086	2.561729414	1.935544425	0.052923523	0.293673665	up_regulated
stress	inter	CvsDr	TRINITY_DN29832_c0_g1	22.92320736	5.058616281	2.550953092	1.983029911	0.047364089	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN21961_c0_g1	43.19424393	5.56629362	2.635141083	2.112332298	0.034657957	0.217960043	up_regulated
stress	inter	CvsDr	TRINITY_DN12967_c2_g1	34.18866187	5.64586331	2.661634689	2.121201431	0.033904855	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN140194_c0_g1	11.99490669	5.728767845	2.989236577	1.91646519	0.05530591	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN890011_c0_g1	264.4140188	5.862895365	2.971182362	1.973253288	0.048466712	0.285751656	up_regulated
stress	inter	CvsDr	TRINITY_DN7807_c0_g1	117.6742147	6.215297743	2.612404116	2.379148656	0.017352676	0.116923986	up_regulated
stress	inter	CvsDr	TRINITY_DN11388_c0_g1	17.95321033	6.314406136	2.936309799	2.150456378	0.031519133	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN626_c0_g2	180.5643059	6.325165069	2.431337323	2.601516873	0.00928125	0.09027918	up_regulated
stress	inter	CvsDr	TRINITY_DN31886_c0_g3	36.47615285	6.347124307	2.70215281	2.348913904	0.01882826	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN48993_c0_g1	18.49968156	6.359209643	2.950182075	2.155531246	0.031120286	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN35789_c0_g1	6.923524624	6.404810438	3.300465483	1.940577919	0.052309495	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN31886_c0_g2	6.944122109	6.407974794	3.299694975	1.941990046	0.052138305	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN7601_c0_g1	7.110123228	6.443455388	3.3068735	1.948503742	0.051354718	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN15730_c2_g1	7.510580213	6.520342119	3.425919443	1.903238598	0.057009419	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN43163_c0_g1	7.510580213	6.520342119	3.425919443	1.903238598	0.057009419	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN62753_c0_g1	7.64204865	6.546609828	3.259968338	2.008182028	0.044623952	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN33856_c0_g1	7.676581332	6.553003686	3.259698549	2.010309722	0.044398419	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN143_c0_g1	7.773517087	6.572453885	3.328020663	1.974883737	0.048281343	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN12925_c0_g1	9.50742408	6.86071887	3.324794184	2.063501826	0.039064974	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN233774_c0_g1	10.44707939	6.996883684	3.675237011	1.903791147	0.05693739	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN713837_c0_g1	203.2799762	7.013963855	2.980071432	2.353622728	0.01859147	0.122357811	up_regulated
stress	inter	CvsDr	TRINITY_DN228084_c0_g1	10.73727604	7.036432374	3.670142343	1.917209665	0.055211301	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN355739_c0_g1	11.31766934	7.112419183	3.660831619	1.94284248	0.052035193	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN2512_c1_g1	11.56000873	7.144424761	3.18340637	2.244270423	0.024815007	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN279814_c0_g1	11.57333331	7.144536811	3.347514667	2.134280958	0.032819789	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN372841_c0_g1	12.47845594	7.253348054	3.6451434	1.989866312	0.046605664	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN26578_c0_g1	13.12852522	7.327934401	3.159589494	2.319267872	0.020380516	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN880286_c0_g1	13.63924254	7.381728414	3.632516909	2.032124997	0.042141003	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN56381_c0_g1	14.57284618	7.47857363	3.61629799	2.068019187	0.038638219	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN713303_c0_g1	14.57284618	7.47857363	3.61629799	2.068019187	0.038638219	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN36486_c0_g1	14.82789953	7.50258389	3.12597278	2.400079725	0.016391501	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN12967_c0_g2	15.63608641	7.578825557	3.314781206	2.286372791	0.022232459	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN15239_c0_g4	17.12160234	7.70990927	3.606615662	2.137713023	0.032540042	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN14319_c0_g1	17.6408138	7.754101247	3.59606546	2.156273665	0.031062302	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN78892_c0_g1	17.99219229	7.781486726	3.602054719	2.160291093	0.030750142	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN25555_c0_g1	20.96444538	8.003045285	3.582402538	2.23398828	0.025483844	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN78733_c0_g1	24.20385517	8.209422751	3.169332694	2.590268534	0.009590109	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN1089_c0_g1	26.30551832	8.330399504	3.087444039	2.698154006	0.006972518	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN37227_c1_g1	28.16301023	8.42806171	3.132589505	2.690445619	0.007135666	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN1281249_c0_g1	82.29147805	8.523753993	3.36979272	2.529459436	0.011423838	0.09027918	up_regulated
stress	inter	CvsDr	TRINITY_DN2512_c4_g1	31.09345773	8.571322801	3.019539165	2.838619515	0.004530915	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN36029_c0_g1	31.92163148	8.608865409	3.571137846	2.410678551	0.015922875	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN52933_c0_g1	88.3328655	8.627818272	2.93102096	2.943622168	0.003243958	0.043551813	up_regulated
stress	inter	CvsDr	TRINITY_DN33386_c1_g1	33.53861584	8.680154312	3.102507388	2.79778683	0.005145406	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN551_c0_g1	34.18263345	8.708205859	3.059191346	2.846571161	0.004419285	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN506168_c0_g1	36.30428347	8.795042138	3.560919144	2.469879765	0.013515847	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN1161095_c0_g1	40.19437316	8.941949554	3.114512651	2.871058993	0.004090992	0.050336985	up_regulated
stress	inter	CvsDr	TRINITY_DN16314_c0_g1	49.59880981	9.245130452	3.559381301	2.597398162	0.009393297	0.09027918	up_regulated
stress	inter	CvsDr	TRINITY_DN1343017_c0_g1	50.02713718	9.257613663	3.159202801	2.930363844	0.003385653	0.043551813	up_regulated
stress	inter	CvsDr	TRINITY_DN17761_c0_g2	58.32952661	9.478683702	3.568200642	2.656432374	0.007897229	0.089396631	up_regulated
stress	inter	CvsDr	TRINITY_DN14781_c0_g1	75.5305203	9.851834414	3.004577374	3.278941824	0.001041971	0.021062702	up_regulated
stress	inter	CvsDr	TRINITY_DN907409_c0_g1	92.40006787	10.14240402	3.186676654	3.182752791	0.001458821	0.025433562	up_regulated
stress	inter	CvsDr	TRINITY_DN1360_c0_g1	169.5742762	11.01854574	3.26866332	3.370963804	0.000749057	0.016306394	up_regulated
stress	inter	CvsDr	TRINITY_DN1384_c0_g1	575.6377229	12.781719	3.287145289	3.888394906	0.000100909	0.002379779	up_regulated
stress	inter	CvsDr	TRINITY_DN73751_c0_g1	77.48250549	22.60051361	3.573165298	6.325068034	2.53E-10	7.16E-09	up_regulated
stress	inter	CvsDr	TRINITY_DN2745_c0_g1	350.0039723	24.42766168	3.623773951	6.740945216	1.57E-11	7.42E-10	up_regulated
stress	inter	CvsDxD	TRINITY_DN725520_c0_g1	405.0287167	-25.05156713	3.965301982	-6.317694654	2.65E-10	2.54E-08	down_regulated
stress	inter	CvsDxD	TRINITY_DN221_c0_g2	721.6952017	-10.18098221	3.260005733	-3.122995187	0.001790207	0.018349619	down_regulated
stress	inter	CvsDxD	TRINITY_DN1351665_c0_g1	66.51423874	-9.15243962	3.904374341	-2.34415013	0.019070493	0.088277929	down_regulated
stress	inter	CvsDxD	TRINITY_DN34378_c0_g1	44.63746254	-8.576989655	3.897809078	-2.200464282	0.027773971	0.116377466	down_regulated
stress	inter	CvsDxD	TRINITY_DN38017_c0_g1	39.44594678	-8.39896981	3.88831681	-2.160052851	0.030768578	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN55012_c0_g1	34.5387955	-8.207341337	3.888367635	-2.110742118	0.034794484	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN31824_c1_g1	31.3788103	-8.068466543	3.897865772	-2.069970342	0.038455122	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN2160_c0_g1	208.2918834	-7.384865249	3.555952829	-2.076761308	0.037823595	0.150769052	down_regulated
stress	inter	CvsDxD	TRINITY_DN26986_c0_g1	13.91216663	-6.894867347	3.414138156	-2.019504494	0.043434811	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN1174_c0_g2	48.97730196	-5.643469991	2.675028989	-2.109685545	0.03488545	0.141015835	down_regulated
stress	inter	CvsDxD	TRINITY_DN44292_c0_g2	48.45882404	5.761297339	3.053411457	1.886839497	0.059181917	0.21500266	up_regulated
stress	inter	CvsDxD	TRINITY_DN7195_c0_g1	167.4208606	6.066916093	3.046794282	1.991245726	0.046453879	0.175424517	up_regulated
stress	inter	CvsDxD	TRINITY_DN33171_c0_g1	223.5344663	6.491032922	2.909778926	2.230764978	0.025696701	0.111273576	up_regulated
stress	inter	CvsDxD	TRINITY_DN52933_c0_g1	19.51177419	6.591416917	3.127544298	2.107537509	0.035071014	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1343017_c0_g1	9.762168962	7.021132393	3.548157951	1.978810552	0.04783734	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN33386_c1_g1	9.965575887	7.051099616	3.451850653	2.042701242	0.041082022	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN66972_c0_g1	9.99302314	7.055623701	3.4697987	2.033438914	0.042008199	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN907409_c0_g1	10.54060473	7.132681689	3.535634842	2.017369442	0.043656974	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN511599_c0_g1	10.72430908	7.156795168	3.508346342	2.039934052	0.041356895	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN5404_c0_g1	83.51719869	7.181667016	3.502434763	2.050478453	0.040317764	0.154282645	up_regulated
stress	inter	CvsDxD	TRINITY_DN43163_c0_g1	13.29400204	7.467134208	3.388387603	2.20374263	0.027542449	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN291433_c0_g1	13.38972656	7.477105216	3.911551256	1.911544737	0.055934613	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN363_c2_g1	13.38972656	7.477105216	3.911551256	1.911544737	0.055934613	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN715182_c0_g1	13.71630525	7.511876942	3.90925762	1.92156099	0.054661021	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN390661_c0_g1	14.04288395	7.545830329	3.907099738	1.93131244	0.053444426	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN896959_c0_g1	14.04288395	7.545830329	3.907099738	1.93131244	0.053444426	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN2512_c4_g1	14.31878135	7.574489488	3.433600877	2.205990085	0.027384694	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN384052_c0_g1	14.36946265	7.579003081	3.905067794	1.940812165	0.052281065	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN588089_c0_g1	14.36946265	7.579003081	3.905067794	1.940812165	0.052281065	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1331259_c0_g1	14.52218828	7.594809939	3.900944631	1.946915595	0.051544855	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN11388_c0_g1	39.17334838	7.60476302	3.252936264	2.337814947	0.01939685	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN65477_c0_g1	14.69604134	7.61142984	3.903152865	1.950072186	0.051167516	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN14781_c0_g1	15.1028552	7.650970878	3.385551733	2.259888929	0.023828145	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN33451_c0_g1	16.00235613	7.734306381	3.89651079	1.984931339	0.047152111	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN741768_c0_g1	16.32893483	7.763457198	3.895072018	1.993148564	0.046245182	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN890758_c0_g1	16.98209222	7.820049255	3.892422051	2.009044536	0.044532411	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1346363_c0_g1	17.30867092	7.847534227	3.891201558	2.016738046	0.043722857	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN61338_c0_g1	17.61201557	7.873061508	3.886920616	2.025526705	0.042813309	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN75126_c0_g1	17.96182831	7.900980588	3.888948925	2.031649359	0.042189166	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN56597_c0_g1	18.22998103	7.922806444	3.884888595	2.039390899	0.04141103	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN809130_c0_g1	18.28840701	7.926979348	3.887909443	2.038879626	0.041462043	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN922286_c0_g1	19.15692921	7.994348427	3.882197129	2.059232996	0.039471924	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN39930_c0_g1	19.2681431	8.002277419	3.885099705	2.059735406	0.039423843	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN551244_c0_g1	19.59472179	8.02652808	3.884256595	2.06642581	0.038788291	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN193763_c0_g1	20.70184286	8.10622458	3.87850665	2.090037561	0.036614426	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN508266_c0_g1	20.70184286	8.10622458	3.87850665	2.090037561	0.036614426	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1128682_c0_g1	22.86050876	8.248946442	3.877830892	2.127206336	0.033402943	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1297832_c0_g1	22.86050876	8.248946442	3.877830892	2.127206336	0.033402943	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN56151_c0_g1	24.16682354	8.329125078	3.876051388	2.148868589	0.031644818	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN568874_c0_g1	25.02760107	8.379948539	3.871884679	2.164307368	0.030440768	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1149599_c0_g1	27.75918921	8.529081384	3.872718572	2.202349906	0.027640601	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN21213_c0_g1	28.7389253	8.57912632	3.872113192	2.21561868	0.026717628	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN897902_c0_g1	30.02764412	8.642412811	3.541438224	2.440368083	0.014672304	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN62753_c0_g1	31.0073802	8.688737663	3.541432351	2.453452954	0.014149205	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN20105_c0_g2	32.00471226	8.734416698	3.870765391	2.256508937	0.024038776	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN213_c0_g2	581.7845229	8.771971781	3.127403536	2.804873654	0.005033631	0.042489765	up_regulated
stress	inter	CvsDxD	TRINITY_DN53446_c0_g1	34.29076314	8.83395979	3.870294781	2.282503088	0.022459655	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN350683_c0_g1	36.15097932	8.910405889	3.866979304	2.304229009	0.021209792	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN101850_c0_g1	36.25023532	8.914134978	3.87012225	2.303321291	0.021260773	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN755426_c0_g1	42.94859937	9.158960534	3.867497384	2.368187907	0.017875455	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN541786_c0_g1	44.74128143	9.217768017	3.870961204	2.38126076	0.017253494	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN52786_c0_g1	45.06786012	9.228260895	3.871028621	2.383929905	0.017128865	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN905987_c0_g1	46.37417491	9.269485568	3.871316296	2.394401506	0.016647514	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN928375_c0_g1	48.81927122	9.343785804	3.868806205	2.415159951	0.015728301	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN494488_c0_g1	64.57739041	9.747337054	3.873906891	2.516151608	0.011864413	0.078535939	up_regulated
stress	inter	CvsDxD	TRINITY_DN26077_c0_g1	69.56126236	9.854468201	3.878775714	2.540613051	0.011065831	0.075616514	up_regulated
stress	inter	CvsDxD	TRINITY_DN107416_c0_g2	70.21441976	9.867951761	3.879011306	2.543934777	0.010961155	0.075616514	up_regulated
stress	inter	CvsDxD	TRINITY_DN8217_c0_g1	70.21441976	9.867951761	3.879011306	2.543934777	0.010961155	0.075616514	up_regulated
stress	inter	CvsDxD	TRINITY_DN2872_c1_g1	72.17389194	9.907662673	3.87971932	2.553706043	0.01065832	0.075616514	up_regulated
stress	inter	CvsDxD	TRINITY_DN6859_c0_g1	102.8546934	10.41874909	3.570667113	2.917871858	0.003524291	0.034878333	up_regulated
stress	inter	CvsDxD	TRINITY_DN5243_c0_g1	179.8744779	11.22514724	3.501193977	3.206091211	0.001345513	0.014567711	up_regulated
stress	inter	CvsDxD	TRINITY_DN6859_c0_g2	76.74599369	22.70623482	3.881372986	5.850052263	4.91E-09	5.64E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN29373_c0_g1	83.73431963	22.76507247	3.880828123	5.866034709	4.46E-09	5.57E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN11181_c0_g1	90.13572025	22.92393512	3.886142983	5.898891322	3.66E-09	5.35E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN13658_c0_g1	123.284109	22.9838824	3.893932844	5.902485564	3.58E-09	5.35E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN68917_c1_g1	105.1583403	23.14421386	3.89123765	5.947777017	2.72E-09	5.20E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN6710_c0_g1	117.5683308	23.29587586	3.895196658	5.980667449	2.22E-09	5.20E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN19981_c0_g1	187.2435339	23.94576319	3.910635959	6.123240168	9.17E-10	2.92E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN5378_c0_g1	432.575821	24.33103179	3.948333149	6.16235532	7.17E-10	2.85E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN4654_c0_g1	490.3555913	25.29069942	3.954234534	6.395852143	1.60E-10	2.29E-08	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


HSP20_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

HSP20_reg

HSP20_reg_final <- HSP20_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

HSP20_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

HSP20_reg_final_plot <- HSP20_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                                 panel.background = element_rect(fill = "white"), 
                                                                                                 panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                                 axis.title.x = element_text(color="black", vjust=1),
                                                                                                 axis.title.y = element_text(color="black" , vjust=1)) 






HSP20_reg_final_plot


jpeg("HSP20_regulation_inter.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(HSP20_reg_final_plot)
dev.off()





################ heat shock protein 20 alpha crystallin family under


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	under	CvsDi	TRINITY_DN12272_c0_g1	128.2801401	-24.10771384	3.246471037	-7.425821319	1.12E-13	1.21E-11	down_regulated
stress	under	CvsDi	TRINITY_DN27953_c0_g1	97.13393322	-23.72181938	3.23755358	-7.327081635	2.35E-13	2.29E-11	down_regulated
stress	under	CvsDi	TRINITY_DN14706_c0_g1	99.77344228	-23.62409023	3.238318852	-7.29517114	2.98E-13	2.63E-11	down_regulated
stress	under	CvsDi	TRINITY_DN748627_c0_g1	101.0931968	-23.47815193	3.238701939	-7.249247498	4.19E-13	3.14E-11	down_regulated
stress	under	CvsDi	TRINITY_DN813_c0_g1	700.0941208	-13.18839661	2.651103637	-4.974681648	6.54E-07	3.97E-05	down_regulated
stress	under	CvsDi	TRINITY_DN1025768_c0_g1	64.18200735	-9.740692077	3.244578718	-3.00214386	0.002680855	0.068643986	down_regulated
stress	under	CvsDi	TRINITY_DN39912_c0_g1	43.81585031	-9.19099947	3.228512281	-2.846821901	0.004415806	0.089020367	down_regulated
stress	under	CvsDi	TRINITY_DN1405_c2_g1	37.72020771	-8.974156794	2.713634402	-3.307061845	0.000942801	0.036166987	down_regulated
stress	under	CvsDi	TRINITY_DN997627_c0_g1	30.61830504	-8.674201906	3.237568598	-2.679233395	0.007379094	0.112025547	down_regulated
stress	under	CvsDi	TRINITY_DN274_c0_g1	30.64546664	-8.673724662	2.801810655	-3.095756898	0.001963112	0.061616398	down_regulated
stress	under	CvsDi	TRINITY_DN100125_c0_g1	29.29855051	-8.61067458	3.239497572	-2.658027793	0.007859941	0.114145115	down_regulated
stress	under	CvsDi	TRINITY_DN727217_c0_g1	28.50669779	-8.571170756	3.240800435	-2.644769688	0.008174654	0.116969675	down_regulated
stress	under	CvsDi	TRINITY_DN24985_c0_g1	23.08665527	-8.264667446	2.934078974	-2.816784251	0.00485071	0.092543934	down_regulated
stress	under	CvsDi	TRINITY_DN89607_c0_g1	22.08421303	-8.200796258	2.832317042	-2.895437247	0.003786307	0.084203407	down_regulated
stress	under	CvsDi	TRINITY_DN1245700_c0_g1	21.47150421	-8.15994316	2.969255698	-2.748144313	0.005993362	0.103461472	down_regulated
stress	under	CvsDi	TRINITY_DN206411_c0_g1	19.90161267	-8.053336963	2.885697648	-2.790776424	0.005258178	0.094615278	down_regulated
stress	under	CvsDi	TRINITY_DN1760029_c0_g1	18.47656338	-7.946034937	3.273884916	-2.427096596	0.015220203	0.151114871	down_regulated
stress	under	CvsDi	TRINITY_DN14038_c0_g1	17.94866157	-7.904252226	3.27707179	-2.411986289	0.015865878	0.155934338	down_regulated
stress	under	CvsDi	TRINITY_DN44060_c0_g1	17.22922998	-7.843802567	2.806495802	-2.794874149	0.005191992	0.094615278	down_regulated
stress	under	CvsDi	TRINITY_DN606547_c0_g1	15.30915252	-7.67499075	3.297182792	-2.327741965	0.019925809	NA	down_regulated
stress	under	CvsDi	TRINITY_DN459349_c0_g1	14.78125071	-7.624416828	3.302258513	-2.30884917	0.020951952	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1381219_c0_g1	14.52540166	-7.595836254	3.321153168	-2.287108083	0.022189515	NA	down_regulated
stress	under	CvsDi	TRINITY_DN372122_c0_g1	14.18760163	-7.561853188	3.324779266	-2.274392549	0.022942395	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1647317_c0_g1	13.98939799	-7.54506614	3.31072361	-2.27897796	0.022668375	NA	down_regulated
stress	under	CvsDi	TRINITY_DN490_c1_g1	36.25466052	-7.472933418	2.635434025	-2.835560802	0.004574531	0.089020367	down_regulated
stress	under	CvsDi	TRINITY_DN54089_c0_g1	12.78446809	-7.412777874	2.869496207	-2.583302901	0.009785936	NA	down_regulated
stress	under	CvsDi	TRINITY_DN105714_c0_g1	12.66964346	-7.40226905	3.327588506	-2.224514551	0.026113845	NA	down_regulated
stress	under	CvsDi	TRINITY_DN61969_c0_g1	12.57387857	-7.390268102	2.877792956	-2.568033286	0.010227733	NA	down_regulated
stress	under	CvsDi	TRINITY_DN189467_c0_g1	12.49860143	-7.378787479	3.346345298	-2.205028717	0.027452079	NA	down_regulated
stress	under	CvsDi	TRINITY_DN112631_c0_g1	12.14174165	-7.340940635	3.335513166	-2.20084295	0.027747144	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1266_c0_g1	100.2001264	-7.324709072	2.695209748	-2.717676826	0.006574202	0.107184558	down_regulated
stress	under	CvsDi	TRINITY_DN297102_c0_g1	11.87779075	-7.309269803	3.339773613	-2.188552474	0.028629383	NA	down_regulated
stress	under	CvsDi	TRINITY_DN40893_c0_g1	11.42231006	-7.249236037	2.922059837	-2.480865021	0.013106399	NA	down_regulated
stress	under	CvsDi	TRINITY_DN82726_c0_g1	11.34988894	-7.2437613	3.348960708	-2.162987844	0.030542115	NA	down_regulated
stress	under	CvsDi	TRINITY_DN78407_c0_g1	11.08593803	-7.209856737	3.353919263	-2.149681066	0.031580451	NA	down_regulated
stress	under	CvsDi	TRINITY_DN65613_c0_g1	10.55803622	-7.139557724	3.364657906	-2.121926782	0.033843887	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1561079_c0_g1	10.44178358	-7.123911261	3.036962472	-2.345735691	0.018989568	NA	down_regulated
stress	under	CvsDi	TRINITY_DN2871_c0_g4	10.40890995	-7.115219956	2.940609727	-2.419641032	0.015535835	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1584271_c0_g1	9.766183503	-7.027232526	3.38315219	-2.077125748	0.037789955	NA	down_regulated
stress	under	CvsDi	TRINITY_DN24985_c5_g1	9.722351989	-7.015560292	3.123076807	-2.246361753	0.024680846	NA	down_regulated
stress	under	CvsDi	TRINITY_DN105581_c0_g1	9.238281692	-6.947172039	3.397388729	-2.044856386	0.040869019	NA	down_regulated
stress	under	CvsDi	TRINITY_DN37724_c0_g1	23.84611195	-6.86312022	3.098992426	-2.214629556	0.026785502	0.19395664	down_regulated
stress	under	CvsDi	TRINITY_DN450160_c0_g1	8.710379881	-6.862404758	3.413470655	-2.010389264	0.044390006	NA	down_regulated
stress	under	CvsDi	TRINITY_DN693651_c0_g1	8.446428976	-6.818075505	3.422310011	-1.992243684	0.046344327	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1574_c0_g1	23.1480397	-6.804049427	2.633581156	-2.583573099	0.009778274	0.128571094	down_regulated
stress	under	CvsDi	TRINITY_DN413820_c0_g1	8.350664086	-6.798752964	2.982549458	-2.279510553	0.022636733	NA	down_regulated
stress	under	CvsDi	TRINITY_DN575_c0_g1	385.7771751	-6.792848828	1.98149024	-3.428151546	0.000607706	0.027878898	down_regulated
stress	under	CvsDi	TRINITY_DN1068_c0_g1	8.075755301	-6.748105726	3.044769321	-2.216294575	0.026671334	NA	down_regulated
stress	under	CvsDi	TRINITY_DN331873_c0_g1	8.055267553	-6.747957222	2.999981554	-2.249332904	0.024491324	NA	down_regulated
stress	under	CvsDi	TRINITY_DN295_c0_g1	42.8871898	-6.702160757	2.327748142	-2.879246528	0.003986266	0.084203407	down_regulated
stress	under	CvsDi	TRINITY_DN477371_c0_g1	7.654576259	-6.676270586	3.452667708	-1.933655698	0.053155474	NA	down_regulated
stress	under	CvsDi	TRINITY_DN41433_c0_g1	7.357751718	-6.612889135	3.191232181	-2.072205581	0.038246273	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1030866_c0_g1	7.231969209	-6.593656876	3.062130111	-2.153290891	0.031295824	NA	down_regulated
stress	under	CvsDi	TRINITY_DN24472_c0_g1	55.33250204	-6.505538239	2.466092544	-2.637994366	0.008339797	0.117603221	down_regulated
stress	under	CvsDi	TRINITY_DN79947_c0_g1	6.513965626	-6.442297783	3.087214186	-2.086767356	0.036909169	NA	down_regulated
stress	under	CvsDi	TRINITY_DN33906_c0_g1	32.88127076	-6.295143891	2.929723077	-2.148716355	0.031656891	0.209538472	down_regulated
stress	under	CvsDi	TRINITY_DN185381_c0_g1	14.88416705	-6.174468172	2.765588882	-2.232605219	0.02557499	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1882_c1_g1	5.236614604	-6.12878903	3.211942068	-1.908125645	0.056374979	NA	down_regulated
stress	under	CvsDi	TRINITY_DN111151_c0_g1	5.18325322	-6.107398344	3.230726355	-1.890410289	0.058703108	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1574628_c0_g1	5.18325322	-6.107398344	3.230726355	-1.890410289	0.058703108	NA	down_regulated
stress	under	CvsDi	TRINITY_DN8661_c0_g1	13.76205975	-6.041708523	2.813481862	-2.147413354	0.031760388	NA	down_regulated
stress	under	CvsDi	TRINITY_DN132495_c0_g1	12.90874378	-5.95265181	2.803629477	-2.123194901	0.033737523	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1616189_c0_g1	24.69879269	-5.876453132	2.940766246	-1.998272777	0.045687095	0.275267188	down_regulated
stress	under	CvsDi	TRINITY_DN301_c0_g2	108.517521	-5.875058135	2.041148469	-2.878310041	0.00399812	0.084203407	down_regulated
stress	under	CvsDi	TRINITY_DN28255_c0_g1	11.28120684	-5.748751447	2.942267792	-1.953850517	0.050718898	NA	down_regulated
stress	under	CvsDi	TRINITY_DN9135_c0_g1	11.00772606	-5.717181059	2.755301992	-2.074974386	0.037988909	NA	down_regulated
stress	under	CvsDi	TRINITY_DN178514_c0_g1	10.91196117	-5.701025774	2.814453863	-2.025624171	0.042803313	NA	down_regulated
stress	under	CvsDi	TRINITY_DN131121_c0_g1	39.98739005	-5.573858099	2.263948533	-2.462007426	0.013816181	0.146121128	down_regulated
stress	under	CvsDi	TRINITY_DN4855_c0_g1	36.29778941	-5.433128349	2.326974218	-2.334846818	0.019551424	0.164412176	down_regulated
stress	under	CvsDi	TRINITY_DN10044_c0_g2	8.857127737	-5.415207794	2.813227099	-1.92490958	0.054240671	NA	down_regulated
stress	under	CvsDi	TRINITY_DN2871_c0_g2	26.38398645	-5.38882184	2.277514942	-2.366097249	0.017976722	0.162153655	down_regulated
stress	under	CvsDi	TRINITY_DN24837_c0_g1	119.5213666	-5.263122759	2.002875498	-2.627783287	0.008594325	0.117778564	down_regulated
stress	under	CvsDi	TRINITY_DN9798_c0_g1	31.56232042	-5.213745791	2.37390675	-2.196272365	0.028072455	0.197490173	down_regulated
stress	under	CvsDi	TRINITY_DN384079_c0_g1	81.98982961	-5.131301123	2.661995082	-1.927614802	0.053903056	0.301423413	down_regulated
stress	under	CvsDi	TRINITY_DN2123_c2_g2	258.9223264	-5.053241171	1.994936856	-2.533033142	0.011308026	0.132698631	down_regulated
stress	under	CvsDi	TRINITY_DN48378_c0_g1	14.05452145	-5.043064006	2.57405106	-1.959193462	0.050090135	NA	down_regulated
stress	under	CvsDi	TRINITY_DN173378_c0_g1	46.73315833	-4.970223846	2.22739549	-2.231406083	0.025654242	0.187681036	down_regulated
stress	under	CvsDi	TRINITY_DN24323_c0_g1	38.24956028	-4.895728568	2.361841306	-2.072843995	0.038186801	0.238177929	down_regulated
stress	under	CvsDi	TRINITY_DN2108_c0_g1	257.1693286	-4.806378625	2.307106784	-2.083292658	0.037224556	0.23519151	down_regulated
stress	under	CvsDi	TRINITY_DN21648_c0_g1	16.56388516	-4.682098169	2.479253587	-1.888511201	0.058957356	0.320477693	down_regulated
stress	under	CvsDi	TRINITY_DN18_c4_g1	70.97655871	-4.576462078	2.028585178	-2.255987142	0.024071436	0.180165443	down_regulated
stress	under	CvsDi	TRINITY_DN21271_c0_g1	35.31982173	-4.544297063	2.378522772	-1.910554364	0.056061874	0.308181939	down_regulated
stress	under	CvsDi	TRINITY_DN11515_c1_g1	33.391726	-4.096133508	2.084225236	-1.965302712	0.049379212	0.287700438	down_regulated
stress	under	CvsDi	TRINITY_DN490_c0_g1	215.8448136	-3.879612539	1.942769878	-1.99694909	0.045830714	0.275267188	down_regulated
stress	under	CvsDr	TRINITY_DN748627_c0_g1	115.0791413	-24.09823754	3.505493006	-6.874421801	6.22E-12	1.21E-09	down_regulated
stress	under	CvsDr	TRINITY_DN14706_c0_g1	113.5768026	-24.0701056	3.505015312	-6.86733251	6.54E-12	1.21E-09	down_regulated
stress	under	CvsDr	TRINITY_DN12447_c0_g1	81.85167538	-10.19443175	2.946707445	-3.459600907	0.000540977	0.020984195	down_regulated
stress	under	CvsDr	TRINITY_DN97141_c0_g1	79.05008129	-10.1441951	3.223224548	-3.147219485	0.001648312	0.036812297	down_regulated
stress	under	CvsDr	TRINITY_DN4850_c0_g1	73.90417525	-10.04708701	2.950181731	-3.40558241	0.000660231	0.021156092	down_regulated
stress	under	CvsDr	TRINITY_DN1936_c0_g1	59.83904919	-9.74254555	2.958861222	-3.292667286	0.000992418	0.029256494	down_regulated
stress	under	CvsDr	TRINITY_DN26363_c0_g1	47.19505833	-9.400142989	2.981370927	-3.152959903	0.00161624	0.036812297	down_regulated
stress	under	CvsDr	TRINITY_DN1774784_c0_g1	41.35884828	-9.209382649	2.962757706	-3.108381974	0.001881148	0.038511273	down_regulated
stress	under	CvsDr	TRINITY_DN69544_c0_g1	37.5855343	-9.071758355	3.206669024	-2.829028593	0.004668953	0.074804743	down_regulated
stress	under	CvsDr	TRINITY_DN264540_c0_g1	37.01166666	-9.049575476	3.153901892	-2.86932688	0.004113464	0.072181507	down_regulated
stress	under	CvsDr	TRINITY_DN33906_c0_g1	36.95753101	-9.047386838	3.483724576	-2.597044238	0.009402981	0.099501893	down_regulated
stress	under	CvsDr	TRINITY_DN5147_c0_g3	35.09786728	-8.972986959	3.043396922	-2.94834594	0.003194793	0.058932249	down_regulated
stress	under	CvsDr	TRINITY_DN997627_c0_g1	34.85425689	-8.962867825	3.484231372	-2.572408909	0.010099353	0.099501893	down_regulated
stress	under	CvsDr	TRINITY_DN683076_c0_g1	28.57150234	-8.676242568	3.207334506	-2.705125565	0.006827858	0.086631267	down_regulated
stress	under	CvsDr	TRINITY_DN1616189_c0_g1	27.64303133	-8.628513449	3.488707766	-2.473269195	0.013388325	0.113624576	down_regulated
stress	under	CvsDr	TRINITY_DN95445_c0_g1	23.37690405	-8.386650903	2.997829827	-2.79757404	0.005148796	NA	down_regulated
stress	under	CvsDr	TRINITY_DN206411_c0_g1	22.36995126	-8.323316412	3.091097368	-2.692673643	0.007088161	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1760029_c0_g1	21.03274123	-8.234328456	3.500117865	-2.352586048	0.018643376	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1245700_c0_g1	20.90813977	-8.224943171	3.167045228	-2.59704001	0.009403097	NA	down_regulated
stress	under	CvsDr	TRINITY_DN14038_c0_g1	20.43180576	-8.192519529	3.501790448	-2.339523067	0.019308379	NA	down_regulated
stress	under	CvsDr	TRINITY_DN50635_c0_g1	18.6533455	-8.061132922	3.039059747	-2.652508866	0.007989603	NA	down_regulated
stress	under	CvsDr	TRINITY_DN84704_c0_g1	18.40973511	-8.042316692	3.136339245	-2.564236858	0.010340295	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1248128_c0_g1	17.47854239	-7.967265629	3.038796631	-2.621848908	0.008745419	NA	down_regulated
stress	under	CvsDr	TRINITY_DN606547_c0_g1	17.42712845	-7.963104272	3.512797188	-2.266884151	0.023397304	NA	down_regulated
stress	under	CvsDr	TRINITY_DN670357_c0_g1	17.20514248	-7.94449698	3.035930484	-2.616824404	0.008875198	NA	down_regulated
stress	under	CvsDr	TRINITY_DN459349_c0_g1	16.82619298	-7.912493956	3.515672545	-2.25063451	0.024408696	NA	down_regulated
stress	under	CvsDr	TRINITY_DN834672_c0_g1	16.82619298	-7.912493956	3.515672545	-2.25063451	0.024408696	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1112731_c0_g1	16.27939316	-7.864959901	3.174020541	-2.47791714	0.013215183	NA	down_regulated
stress	under	CvsDr	TRINITY_DN105714_c0_g2	15.92478979	-7.833085335	3.520536293	-2.224969347	0.026083296	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1647317_c0_g1	15.92478979	-7.833085335	3.520536293	-2.224969347	0.026083296	NA	down_regulated
stress	under	CvsDr	TRINITY_DN105714_c0_g1	14.42245113	-7.690176626	3.530439675	-2.178248982	0.029387505	NA	down_regulated
stress	under	CvsDr	TRINITY_DN132495_c0_g1	14.23025468	-7.670907105	3.133134154	-2.448317477	0.014352513	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1381219_c0_g1	14.0840288	-7.654829685	3.538470428	-2.163315998	0.030516883	NA	down_regulated
stress	under	CvsDr	TRINITY_DN112631_c0_g1	13.82151566	-7.628798306	3.535175614	-2.157968695	0.030930266	NA	down_regulated
stress	under	CvsDr	TRINITY_DN297102_c0_g1	13.52104793	-7.59710129	3.537740719	-2.147444342	0.031757924	NA	down_regulated
stress	under	CvsDr	TRINITY_DN710736_c0_g1	13.27199415	-7.57003562	3.065875315	-2.469127033	0.013544313	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1698353_c0_g1	13.10414383	-7.55099976	3.075536751	-2.455181118	0.014081362	NA	down_regulated
stress	under	CvsDr	TRINITY_DN152598_c0_g2	13.05545158	-7.546603888	3.12582263	-2.414277706	0.01576644	NA	down_regulated
stress	under	CvsDr	TRINITY_DN82726_c0_g1	12.92011247	-7.531538017	3.543313303	-2.125563667	0.033539608	NA	down_regulated
stress	under	CvsDr	TRINITY_DN189467_c0_g1	12.11881548	-7.437939838	3.557225588	-2.090938473	0.03653358	NA	down_regulated
stress	under	CvsDr	TRINITY_DN89078_c0_g1	11.71824154	-7.390731985	3.55656	-2.07805632	0.037704172	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1584271_c0_g1	11.11730608	-7.314815493	3.564470191	-2.052146631	0.040155414	NA	down_regulated
stress	under	CvsDr	TRINITY_DN105581_c0_g1	10.51637061	-7.234680547	3.573440658	-2.024569942	0.042911543	NA	down_regulated
stress	under	CvsDr	TRINITY_DN727217_c0_g1	32.69370562	-7.222845517	3.261224504	-2.214764886	0.026776207	0.163947225	down_regulated
stress	under	CvsDr	TRINITY_DN664173_c0_g1	10.21045949	-7.190977839	3.120643467	-2.304325347	0.021204387	NA	down_regulated
stress	under	CvsDr	TRINITY_DN450160_c0_g1	9.91543515	-7.149830599	3.583669027	-1.995114656	0.046030376	NA	down_regulated
stress	under	CvsDr	TRINITY_DN693651_c0_g1	9.614967418	-7.105457238	3.589330097	-1.979605399	0.047747886	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1266_c0_g1	97.32549219	-7.093943597	2.907413067	-2.439950373	0.01468928	0.113957886	down_regulated
stress	under	CvsDr	TRINITY_DN24985_c5_g1	9.471463227	-7.082093998	3.292532857	-2.150956211	0.031479656	NA	down_regulated
stress	under	CvsDr	TRINITY_DN158570_c0_g1	9.422770973	-7.076341315	3.184008268	-2.222463235	0.026252016	NA	down_regulated
stress	under	CvsDr	TRINITY_DN825658_c0_g1	9.041099776	-7.016858626	3.294475402	-2.129886483	0.033180986	NA	down_regulated
stress	under	CvsDr	TRINITY_DN413820_c0_g1	8.764978171	-6.971212184	3.140911005	-2.219487331	0.026453588	NA	down_regulated
stress	under	CvsDr	TRINITY_DN477371_c0_g1	8.713564223	-6.963508527	3.608960113	-1.929505539	0.053668133	NA	down_regulated
stress	under	CvsDr	TRINITY_DN491096_c0_g1	8.713564223	-6.963508527	3.608960113	-1.929505539	0.053668133	NA	down_regulated
stress	under	CvsDr	TRINITY_DN510588_c0_g1	8.710842528	-6.96244474	3.144948611	-2.213850081	0.026839094	NA	down_regulated
stress	under	CvsDr	TRINITY_DN331873_c0_g1	8.656706885	-6.953623281	3.152919812	-2.205455164	0.027422171	NA	down_regulated
stress	under	CvsDr	TRINITY_DN954588_c0_g1	8.440164312	-6.917684132	3.306532695	-2.092126336	0.036427216	NA	down_regulated
stress	under	CvsDr	TRINITY_DN664299_c0_g1	8.407653102	-6.910295647	3.209718103	-2.152929144	0.031324248	NA	down_regulated
stress	under	CvsDr	TRINITY_DN40449_c2_g1	8.082839243	-6.854528253	3.165005612	-2.165723886	0.030332293	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1363751_c0_g1	7.947500135	-6.830655729	3.201694432	-2.133450232	0.03288781	NA	down_regulated
stress	under	CvsDr	TRINITY_DN109714_c0_g1	7.920432314	-6.825827124	3.216436283	-2.122170789	0.033823399	NA	down_regulated
stress	under	CvsDr	TRINITY_DN55213_c0_g1	51.93303439	-6.785459168	2.771627526	-2.448185806	0.014357759	0.113781382	down_regulated
stress	under	CvsDr	TRINITY_DN218074_c0_g1	7.619964582	-6.770042685	3.224712712	-2.099425062	0.035779447	NA	down_regulated
stress	under	CvsDr	TRINITY_DN314232_c0_g1	6.964893475	-6.64059383	3.293410481	-2.016327411	0.04376575	NA	down_regulated
stress	under	CvsDr	TRINITY_DN118390_c0_g1	6.799764851	-6.605377144	3.221699194	-2.05027743	0.040337366	NA	down_regulated
stress	under	CvsDr	TRINITY_DN476509_c0_g1	6.718561386	-6.588435871	3.254596477	-2.024348001	0.042934358	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1800759_c0_g1	6.637357922	-6.571217731	3.357187862	-1.957357765	0.05030542	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1327091_c0_g1	6.580500584	-6.557633411	3.222938517	-2.034675305	0.041883555	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1276013_c0_g1	6.391025833	-6.516471712	3.286056689	-1.983067344	0.047359908	NA	down_regulated
stress	under	CvsDr	TRINITY_DN76832_c0_g1	6.361236317	-6.508229402	3.243911976	-2.006290384	0.044825275	NA	down_regulated
stress	under	CvsDr	TRINITY_DN91406_c2_g1	16.15425357	-6.477993919	2.94937858	-2.196392814	0.02806384	NA	down_regulated
stress	under	CvsDr	TRINITY_DN67600_c0_g2	6.198829387	-6.471822699	3.248643388	-1.992161627	0.046353327	NA	down_regulated
stress	under	CvsDr	TRINITY_DN384079_c0_g1	91.77907726	-6.423659356	3.116998724	-2.060847605	0.039317581	0.213066598	down_regulated
stress	under	CvsDr	TRINITY_DN1882_c1_g1	5.790090369	-6.374040854	3.313855984	-1.923451376	0.054423387	NA	down_regulated
stress	under	CvsDr	TRINITY_DN27953_c0_g1	112.0312688	-6.263180118	3.155024019	-1.985144988	0.047128343	0.236241366	down_regulated
stress	under	CvsDr	TRINITY_DN372122_c0_g1	13.90475356	-6.262844513	3.305235173	-1.894825689	0.058115499	NA	down_regulated
stress	under	CvsDr	TRINITY_DN156869_c0_g1	5.324494014	-6.252101317	3.298541928	-1.895413626	0.058037625	NA	down_regulated
stress	under	CvsDr	TRINITY_DN358843_c0_g1	12.65146869	-6.119555493	2.889913868	-2.117556361	0.034212657	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1715_c1_g1	299.4665021	-5.797311398	2.917614049	-1.98700421	0.04692193	0.236241366	down_regulated
stress	under	CvsDr	TRINITY_DN34459_c0_g1	32.3234628	-5.680090177	2.585681777	-2.196747576	0.028038479	0.166648056	down_regulated
stress	under	CvsDr	TRINITY_DN14408_c0_g1	21.30150679	-5.476613483	2.670784202	-2.05056383	0.040309442	NA	down_regulated
stress	under	CvsDr	TRINITY_DN21348_c0_g1	156.7664384	-5.381123481	2.406941691	-2.235668401	0.0253735	0.163733396	down_regulated
stress	under	CvsDr	TRINITY_DN27218_c0_g1	18.45214436	-5.285003434	2.697278633	-1.959383569	0.050067883	NA	down_regulated
stress	under	CvsDr	TRINITY_DN24472_c0_g1	63.32256526	-5.200671073	2.701460419	-1.925133175	0.0542127	0.256120254	down_regulated
stress	under	CvsDr	TRINITY_DN156959_c0_g1	22.60022409	-5.153151897	2.613817915	-1.971503779	0.048666281	NA	down_regulated
stress	under	CvsDr	TRINITY_DN42577_c0_g1	34.54122315	-5.148426505	2.711084249	-1.899028593	0.057560717	0.263545208	down_regulated
stress	under	CvsDr	TRINITY_DN125416_c0_g1	48.81120287	-4.942900415	2.278170947	-2.169679331	0.030031147	0.175649053	down_regulated
stress	under	CvsDr	TRINITY_DN2123_c2_g2	264.8737157	-4.247453413	2.213568571	-1.918826219	0.05500633	0.25821443	down_regulated
stress	under	CvsDxD	TRINITY_DN1715_c1_g1	246.5965537	-24.9601343	3.435578857	-7.265190332	3.73E-13	7.07E-11	down_regulated
stress	under	CvsDxD	TRINITY_DN12272_c0_g1	122.4166753	-24.01799464	3.404872542	-7.05400697	1.74E-12	1.65E-10	down_regulated
stress	under	CvsDxD	TRINITY_DN748627_c0_g1	96.47240049	-23.69212006	3.396068771	-6.976336952	3.03E-12	2.56E-10	down_regulated
stress	under	CvsDxD	TRINITY_DN1025768_c0_g1	63.69494712	-9.700409939	3.40185185	-2.851508639	0.00435123	0.055975998	down_regulated
stress	under	CvsDxD	TRINITY_DN987461_c0_g1	46.59894018	-9.250524957	3.379770266	-2.737027735	0.006199707	0.064459963	down_regulated
stress	under	CvsDxD	TRINITY_DN1774784_c0_g1	40.49495351	-9.046852569	2.880155178	-3.141099007	0.001683151	0.026071669	down_regulated
stress	under	CvsDxD	TRINITY_DN3456_c3_g1	30.47822574	-8.638317649	3.381886118	-2.554289928	0.010640462	0.088322486	down_regulated
stress	under	CvsDxD	TRINITY_DN69870_c0_g1	27.45559178	-8.487735225	3.384387918	-2.5079085	0.012144811	0.092115437	down_regulated
stress	under	CvsDxD	TRINITY_DN1076453_c0_g1	24.43295783	-8.319582528	3.38832339	-2.455368503	0.014074023	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN59598_c0_g1	20.65466538	-8.077415341	3.396398402	-2.378229638	0.017395989	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1796678_c0_g1	49.81367809	-7.926621067	3.193790964	-2.481884744	0.013068954	0.094401759	down_regulated
stress	under	CvsDxD	TRINITY_DN1760029_c0_g1	17.63203142	-7.849355659	3.407003501	-2.303888345	0.021228912	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN14038_c0_g1	17.12825909	-7.807576885	3.409299225	-2.290082615	0.022016528	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN26363_c0_g1	41.15151845	-7.647042097	2.734988604	-2.796005104	0.005173858	0.062332674	down_regulated
stress	under	CvsDxD	TRINITY_DN606547_c0_g1	14.60939746	-7.578340223	3.424065157	-2.213258182	0.026879851	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1381219_c0_g1	14.41517224	-7.555494168	3.443818477	-2.193929273	0.028240495	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN459349_c0_g1	14.10562514	-7.527771649	3.427852087	-2.196060815	0.028087591	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN91406_c2_g1	13.60001785	-7.475435409	3.065806198	-2.438326145	0.014755454	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1647317_c0_g1	13.34996665	-7.4484327	3.43421015	-2.168892518	0.030090845	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1266_c0_g1	99.38774775	-7.40188771	2.829691241	-2.615793413	0.00890204	0.08043629	down_regulated
stress	under	CvsDxD	TRINITY_DN1698353_c0_g1	12.74449431	-7.378092023	2.995315128	-2.463210617	0.0137699	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN82726_c0_g1	10.83110502	-7.147174814	3.463432353	-2.063610339	0.039054676	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN100125_c0_g1	28.18463092	-7.013537743	3.189856252	-2.198700251	0.027899243	0.139462498	down_regulated
stress	under	CvsDxD	TRINITY_DN1273360_c0_g1	9.721860349	-6.986465861	3.500392276	-1.995909404	0.045943785	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN24911_c0_g1	9.642179871	-6.975253103	3.052171009	-2.28534151	0.022292811	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN105581_c0_g1	8.816015711	-6.850646213	3.501245129	-1.956631416	0.050390819	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN122919_c1_g1	8.816015711	-6.850646213	3.501245129	-1.956631416	0.050390819	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN384079_c0_g1	76.74542171	-6.829967064	3.044600681	-2.243304715	0.024877172	0.131374158	down_regulated
stress	under	CvsDxD	TRINITY_DN664299_c0_g1	8.382749055	-6.772283449	3.130320081	-2.163447595	0.03050677	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN40449_c2_g1	7.382544248	-6.592312873	3.09488751	-2.130065423	0.033166212	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1363751_c0_g1	6.965792245	-6.510436836	3.138570032	-2.074332186	0.038048471	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN434628_c0_g1	6.875101999	-6.486578612	3.146766583	-2.06134724	0.039269924	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN218074_c0_g1	6.630555681	-6.439704902	3.164536103	-2.034960162	0.041854882	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN43549_c2_g1	6.289814234	-6.359402584	3.151127786	-2.018135415	0.04357716	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN76832_c0_g1	6.121278471	-6.319464212	3.17020469	-1.993393118	0.046218417	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN265268_c0_g1	5.623011029	-6.201908878	3.211792354	-1.930980647	0.053485446	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1276013_c0_g1	5.539660628	-6.180957089	3.234097641	-1.91118444	0.055980883	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1785302_c0_g1	5.450805344	-6.15230036	3.206828526	-1.918499948	0.055047649	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN6633_c0_g2	5.369289904	-6.133223278	3.20351681	-1.914528202	0.055552699	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN19419_c0_g1	5.202589103	-6.089036342	3.226031089	-1.887469827	0.059097161	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN205816_c1_g1	12.88090584	-5.870880987	3.003865238	-1.954442201	0.050648944	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN14706_c0_g1	96.94190519	-5.777345279	3.032321458	-1.905254888	0.056746947	0.234081154	down_regulated
stress	under	CvsDxD	TRINITY_DN95445_c0_g1	21.19456431	-5.728104894	2.605826156	-2.198191496	0.027935462	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN39912_c0_g1	42.6478446	-5.64841651	2.765751164	-2.042272126	0.041124545	0.185794821	down_regulated
stress	under	CvsDxD	TRINITY_DN1587742_c0_g1	22.35362005	-5.633608193	2.605025025	-2.162592735	0.030572518	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN46812_c0_g1	9.702580844	-5.459533584	2.853555886	-1.913238711	0.0557175	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN61190_c0_g1	9.44885972	-5.418735924	2.874645842	-1.885009919	0.059428499	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN2871_c0_g2	26.07776873	-5.251644238	2.585772895	-2.030976598	0.042257369	0.189783093	down_regulated
stress	under	CvsDxD	TRINITY_DN12447_c0_g1	73.74284088	-5.059871682	2.198337091	-2.30168144	0.021353144	0.121857414	down_regulated
stress	under	CvsDxD	TRINITY_DN206411_c0_g1	19.63263992	-5.027213901	2.659034134	-1.890616535	0.058675551	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1947_c1_g1	30.77231813	-4.280941629	2.264158693	-1.890742748	0.058658693	0.238085284	down_regulated
stress	under	CvsDi	TRINITY_DN5665_c0_g1	49.28015048	4.051293028	2.089481205	1.938899004	0.052513637	0.298805667	up_regulated
stress	under	CvsDi	TRINITY_DN2753_c0_g1	65.71174451	4.487936659	2.093622163	2.143623017	0.032063105	0.210793251	up_regulated
stress	under	CvsDi	TRINITY_DN1032195_c0_g1	24.53704083	4.669223525	2.23091421	2.092964178	0.036352352	0.231181953	up_regulated
stress	under	CvsDi	TRINITY_DN114579_c0_g1	112.0537239	4.704472507	2.126017218	2.212810163	0.026910736	0.19395664	up_regulated
stress	under	CvsDi	TRINITY_DN46857_c0_g1	27.40456431	4.736882328	2.450682722	1.932882737	0.053250645	0.299496402	up_regulated
stress	under	CvsDi	TRINITY_DN39791_c4_g1	77.02086657	4.762283378	2.100494948	2.26721963	0.023376812	0.179099514	up_regulated
stress	under	CvsDi	TRINITY_DN18_c0_g1	250.0812769	5.066161357	2.178270745	2.325772114	0.020030711	0.164412176	up_regulated
stress	under	CvsDi	TRINITY_DN171895_c0_g2	18.96813369	5.077656532	2.651074624	1.915320107	0.055451693	0.306559643	up_regulated
stress	under	CvsDi	TRINITY_DN17614_c0_g1	11.61544448	5.166966057	2.731206555	1.891825445	0.058514242	NA	up_regulated
stress	under	CvsDi	TRINITY_DN3316_c0_g2	12.7763601	5.427836534	2.733612163	1.985591302	0.047078723	NA	up_regulated
stress	under	CvsDi	TRINITY_DN118144_c0_g1	14.42090135	5.487354008	2.739224892	2.003250637	0.045150393	NA	up_regulated
stress	under	CvsDi	TRINITY_DN198375_c0_g1	13.95406962	5.558608085	2.866751767	1.938991771	0.05250234	NA	up_regulated
stress	under	CvsDi	TRINITY_DN160_c5_g2	14.17712478	5.58078833	2.720898027	2.051083236	0.040258842	NA	up_regulated
stress	under	CvsDi	TRINITY_DN3316_c0_g1	29.34564492	5.584895492	2.369036074	2.357454812	0.018400695	0.162153655	up_regulated
stress	under	CvsDi	TRINITY_DN15147_c0_g1	33.30849709	5.636381757	2.676453874	2.105914027	0.035211821	0.227540233	up_regulated
stress	under	CvsDi	TRINITY_DN63653_c0_g1	36.91377179	5.796193078	2.414623279	2.400454401	0.016374731	0.157289673	up_regulated
stress	under	CvsDi	TRINITY_DN473108_c0_g1	6.346750012	5.852237493	3.083232869	1.898084816	0.05768491	NA	up_regulated
stress	under	CvsDi	TRINITY_DN6633_c0_g1	36.85925062	5.919448064	2.465390823	2.40101813	0.016349526	0.157289673	up_regulated
stress	under	CvsDi	TRINITY_DN74308_c0_g1	7.154732423	6.025162693	3.03337604	1.986289406	0.047001198	NA	up_regulated
stress	under	CvsDi	TRINITY_DN400370_c0_g1	7.40011437	6.073687776	3.03909625	1.998517743	0.045660558	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1270872_c0_g1	8.011915048	6.188806296	3.180391438	1.94592597	0.051663633	NA	up_regulated
stress	under	CvsDi	TRINITY_DN39791_c3_g1	8.117551366	6.207474451	2.991450093	2.075072041	0.037979859	NA	up_regulated
stress	under	CvsDi	TRINITY_DN12081_c0_g1	8.506296888	6.274541452	3.168113524	1.980529235	0.047644092	NA	up_regulated
stress	under	CvsDi	TRINITY_DN47809_c0_g1	26.15189199	6.346636634	3.092201364	2.052465505	0.040124444	0.247095469	up_regulated
stress	under	CvsDi	TRINITY_DN310421_c0_g1	9.605243516	6.450380361	3.416236512	1.88815392	0.05900529	NA	up_regulated
stress	under	CvsDi	TRINITY_DN2082_c0_g1	29.10794832	6.524259638	2.598486559	2.510792144	0.01204606	0.136377219	up_regulated
stress	under	CvsDi	TRINITY_DN1264070_c0_g1	10.18655283	6.534937802	2.918014904	2.239514882	0.025122434	NA	up_regulated
stress	under	CvsDi	TRINITY_DN33742_c0_g1	10.60578972	6.593318529	3.392884269	1.943278345	0.051982536	NA	up_regulated
stress	under	CvsDi	TRINITY_DN74_c1_g2	10.78295304	6.616744642	3.064558274	2.159118558	0.03084097	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1112984_c0_g1	11.0060082	6.646750681	3.384828052	1.963689316	0.049566132	NA	up_regulated
stress	under	CvsDi	TRINITY_DN357595_c0_g1	33.22362445	6.711981855	2.709152978	2.477520431	0.013229883	0.144015439	up_regulated
stress	under	CvsDi	TRINITY_DN1784945_c0_g1	11.99508144	6.770747951	2.878117795	2.352491605	0.018648111	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1596896_c0_g1	12.63706392	6.846147191	2.983678814	2.294532226	0.021759946	NA	up_regulated
stress	under	CvsDi	TRINITY_DN42134_c0_g2	12.80699135	6.865363465	3.355387784	2.046071545	0.040749332	NA	up_regulated
stress	under	CvsDi	TRINITY_DN121577_c0_g1	13.02942739	6.889827104	2.940068303	2.343424164	0.019107646	NA	up_regulated
stress	under	CvsDi	TRINITY_DN71348_c1_g1	13.49755493	6.940754237	3.348813179	2.072601207	0.038209409	NA	up_regulated
stress	under	CvsDi	TRINITY_DN134731_c0_g1	13.61497377	6.953664049	3.080630507	2.257221056	0.023994265	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1324417_c0_g1	14.40786527	7.035268874	3.336089379	2.108837046	0.03495865	NA	up_regulated
stress	under	CvsDi	TRINITY_DN301_c0_g1	14.43804708	7.038339302	2.966196438	2.372850028	0.017651429	NA	up_regulated
stress	under	CvsDi	TRINITY_DN13449_c0_g2	14.80808375	7.07479275	3.332011991	2.123279499	0.033730438	NA	up_regulated
stress	under	CvsDi	TRINITY_DN76336_c0_g1	15.01573844	7.094927044	3.068702705	2.312028152	0.020776132	NA	up_regulated
stress	under	CvsDi	TRINITY_DN33577_c0_g3	15.20830223	7.11326271	3.328185231	2.137279693	0.03257525	NA	up_regulated
stress	under	CvsDi	TRINITY_DN89748_c0_g1	15.54392003	7.144412744	2.960588027	2.413173558	0.015814287	NA	up_regulated
stress	under	CvsDi	TRINITY_DN223639_c0_g1	15.98217533	7.184822267	2.833140109	2.53599257	0.011212911	NA	up_regulated
stress	under	CvsDi	TRINITY_DN78823_c0_g1	16.6123753	7.24034378	3.318862129	2.181574136	0.029140977	0.198280917	up_regulated
stress	under	CvsDi	TRINITY_DN1068510_c0_g1	16.60906691	7.240361288	3.316496407	2.183135574	0.029025827	0.198280917	up_regulated
stress	under	CvsDi	TRINITY_DN480450_c0_g1	17.02768468	7.275970948	3.315838302	2.194308131	0.028213265	0.197490173	up_regulated
stress	under	CvsDi	TRINITY_DN106037_c0_g1	17.20939463	7.291581464	3.312181805	2.201443608	0.027704635	0.197490173	up_regulated
stress	under	CvsDi	TRINITY_DN89774_c0_g1	180.465259	7.298554241	2.430490506	3.002914113	0.002674079	0.068643986	up_regulated
stress	under	CvsDi	TRINITY_DN1320548_c0_g1	18.68892221	7.410283147	3.305370232	2.24189202	0.024968352	0.185451957	up_regulated
stress	under	CvsDi	TRINITY_DN2512_c0_g3	19.21048703	7.450263985	3.300160604	2.257545883	0.023973986	0.180165443	up_regulated
stress	under	CvsDi	TRINITY_DN115111_c0_g1	19.41783216	7.465613414	2.78699186	2.678735277	0.007390079	0.112025547	up_regulated
stress	under	CvsDi	TRINITY_DN64994_c0_g1	20.35015974	7.533149375	3.297006921	2.284844878	0.022321925	0.173152754	up_regulated
stress	under	CvsDi	TRINITY_DN10234_c0_g1	20.98097882	7.577350473	2.773567409	2.731987133	0.00629536	0.10561009	up_regulated
stress	under	CvsDi	TRINITY_DN1258812_c0_g1	21.81190715	7.633469615	3.28861091	2.321183571	0.020276938	0.164412176	up_regulated
stress	under	CvsDi	TRINITY_DN75796_c0_g1	22.41223487	7.672636927	3.286440035	2.334634694	0.019562513	0.164412176	up_regulated
stress	under	CvsDi	TRINITY_DN176635_c0_g1	24.21321803	7.784136243	3.280795147	2.372637088	0.017661608	0.16212023	up_regulated
stress	under	CvsDi	TRINITY_DN690549_c0_g1	180.2255498	7.786396449	2.528859366	3.079015209	0.002076861	0.063149548	up_regulated
stress	under	CvsDi	TRINITY_DN1024355_c0_g1	135.3618873	7.81387946	2.361141931	3.309364574	0.00093508	0.036166987	up_regulated
stress	under	CvsDi	TRINITY_DN71348_c0_g1	24.91856294	7.825349772	3.281258881	2.384862047	0.017085527	0.159848247	up_regulated
stress	under	CvsDi	TRINITY_DN1105384_c0_g1	26.26681903	7.901511216	2.745317025	2.878178055	0.003999793	0.084203407	up_regulated
stress	under	CvsDi	TRINITY_DN540229_c0_g1	28.41551207	8.015002971	3.271423999	2.450004333	0.01428545	0.148233181	up_regulated
stress	under	CvsDi	TRINITY_DN1874576_c0_g1	32.21758763	8.196160574	3.266019073	2.509526243	0.012089323	0.136377219	up_regulated
stress	under	CvsDi	TRINITY_DN2512_c0_g1	33.61835231	8.257557285	3.264536374	2.529473205	0.01142339	0.132698631	up_regulated
stress	under	CvsDi	TRINITY_DN2512_c0_g2	34.81900774	8.308180204	3.26343815	2.545836575	0.010901621	0.132698631	up_regulated
stress	under	CvsDi	TRINITY_DN171_c0_g2	35.233698	8.32518263	2.723312189	3.057006341	0.002235595	0.065916194	up_regulated
stress	under	CvsDi	TRINITY_DN1263362_c0_g1	35.44197182	8.333800898	2.937917028	2.836635895	0.004559157	0.089020367	up_regulated
stress	under	CvsDi	TRINITY_DN6035_c1_g1	94.70672845	8.341228282	2.527553125	3.300119867	0.000966435	0.036166987	up_regulated
stress	under	CvsDi	TRINITY_DN1411047_c0_g1	36.21977242	8.365079188	3.262332777	2.564140374	0.01034317	0.132419799	up_regulated
stress	under	CvsDi	TRINITY_DN11599_c0_g1	38.02830103	8.435399818	3.019549293	2.793595666	0.005212561	0.094615278	up_regulated
stress	under	CvsDi	TRINITY_DN1735_c0_g1	40.70031947	8.533202136	3.262134498	2.61583394	0.008900984	0.119167658	up_regulated
stress	under	CvsDi	TRINITY_DN649497_c0_g1	41.8605584	8.573920888	2.899409351	2.957126729	0.003105205	0.075534103	up_regulated
stress	under	CvsDi	TRINITY_DN26933_c0_g1	42.17684823	8.584658009	2.730057328	3.144497342	0.001663724	0.055299736	up_regulated
stress	under	CvsDi	TRINITY_DN6750_c0_g1	129.0080413	8.633527988	3.099704551	2.785274482	0.005348243	0.094615278	up_regulated
stress	under	CvsDi	TRINITY_DN54792_c1_g1	45.06985169	8.680486568	2.886275738	3.007504257	0.002634024	0.068643986	up_regulated
stress	under	CvsDi	TRINITY_DN5340_c3_g1	45.22468822	8.685394764	3.258421307	2.665522333	0.007686883	0.113323284	up_regulated
stress	under	CvsDi	TRINITY_DN171_c1_g1	55.41062192	8.978421416	2.68471852	3.344269185	0.000824996	0.034900938	up_regulated
stress	under	CvsDi	TRINITY_DN19883_c0_g2	57.48417001	9.031470451	2.878725978	3.13731509	0.001705028	0.055299736	up_regulated
stress	under	CvsDi	TRINITY_DN20029_c0_g1	58.44698896	9.055425127	2.970902898	3.048038067	0.002303407	0.065918102	up_regulated
stress	under	CvsDi	TRINITY_DN1250314_c0_g1	76.65692979	9.446695236	2.977537392	3.172653771	0.001510525	0.052490759	up_regulated
stress	under	CvsDi	TRINITY_DN655801_c0_g1	98.62843287	9.810200427	3.037167717	3.23004896	0.00123769	0.044602688	up_regulated
stress	under	CvsDi	TRINITY_DN951000_c0_g1	85.84686392	20.85045914	3.264090792	6.387830628	1.68E-10	1.09E-08	up_regulated
stress	under	CvsDr	TRINITY_DN74_c0_g1	443.684454	4.342444429	2.186076647	1.986409962	0.046987821	0.236241366	up_regulated
stress	under	CvsDr	TRINITY_DN1068_c0_g1	213.8330573	4.676666467	2.458643666	1.902132681	0.057153811	0.263545208	up_regulated
stress	under	CvsDr	TRINITY_DN295_c0_g1	1360.227823	4.963187145	2.520830873	1.96886955	0.048968073	0.239003112	up_regulated
stress	under	CvsDr	TRINITY_DN274_c0_g1	1013.004819	5.015647111	2.571221982	1.950686151	0.051094392	0.245333982	up_regulated
stress	under	CvsDr	TRINITY_DN13622_c0_g1	73.99546446	5.240224711	2.510541213	2.087288862	0.036862031	0.20426554	up_regulated
stress	under	CvsDr	TRINITY_DN19883_c0_g1	65.67440696	5.29578418	2.801406271	1.890402058	0.058704208	0.267067911	up_regulated
stress	under	CvsDr	TRINITY_DN13270_c0_g1	44.74927197	5.487954215	2.768414784	1.982345365	0.0474406	0.236241366	up_regulated
stress	under	CvsDr	TRINITY_DN3316_c0_g1	28.90317125	5.491704713	2.637855506	2.08188231	0.037353221	0.205442716	up_regulated
stress	under	CvsDr	TRINITY_DN2753_c0_g1	163.9570044	5.714035873	2.303282494	2.480822864	0.013107949	0.113624576	up_regulated
stress	under	CvsDr	TRINITY_DN735_c0_g1	221.8021245	5.804567422	2.97782405	1.94926474	0.051263817	0.245333982	up_regulated
stress	under	CvsDr	TRINITY_DN184629_c0_g1	39.5942057	5.907518986	2.9228246	2.021167807	0.043262397	0.22760957	up_regulated
stress	under	CvsDr	TRINITY_DN11435_c1_g1	7.67434719	6.057120656	3.187215192	1.900442955	0.057375014	NA	up_regulated
stress	under	CvsDr	TRINITY_DN6035_c0_g3	127.6859193	6.119541605	2.711635018	2.256771861	0.024022334	0.156676638	up_regulated
stress	under	CvsDr	TRINITY_DN135651_c0_g1	8.048749061	6.121145499	3.197634067	1.914273294	0.055585245	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1486_c0_g1	225.9567295	6.125021803	2.916646722	2.100021836	0.03572692	0.204114266	up_regulated
stress	under	CvsDr	TRINITY_DN1038034_c0_g1	8.535130234	6.205837551	3.1862299	1.947705516	0.05145021	NA	up_regulated
stress	under	CvsDr	TRINITY_DN16791_c0_g1	124.0703944	6.344848367	3.186139623	1.99139056	0.046437966	0.236241366	up_regulated
stress	under	CvsDr	TRINITY_DN1151_c0_g2	169.6584469	6.49598125	2.925113719	2.220761951	0.026367089	0.163947225	up_regulated
stress	under	CvsDr	TRINITY_DN56871_c0_g1	10.84864614	6.551686587	3.315567428	1.976037806	0.048150496	NA	up_regulated
stress	under	CvsDr	TRINITY_DN6035_c0_g2	236.6791914	6.57165289	2.46503864	2.665943155	0.007677267	0.092151384	up_regulated
stress	under	CvsDr	TRINITY_DN1773020_c0_g1	370.9716591	6.668232803	2.991982594	2.228700399	0.025833846	0.163947225	up_regulated
stress	under	CvsDr	TRINITY_DN74308_c0_g1	11.85550656	6.683966956	3.087519953	2.164833607	0.03040043	NA	up_regulated
stress	under	CvsDr	TRINITY_DN160_c5_g2	34.59034046	6.786919432	3.332765067	2.036422999	0.041707898	0.221141876	up_regulated
stress	under	CvsDr	TRINITY_DN4557_c0_g1	429.2263218	6.799610222	2.306525672	2.947988095	0.003198494	0.058932249	up_regulated
stress	under	CvsDr	TRINITY_DN289592_c0_g1	13.34342897	6.855160331	3.55555486	1.92801422	0.053853357	NA	up_regulated
stress	under	CvsDr	TRINITY_DN2512_c0_g1	13.49168929	6.871081383	3.55425423	1.933199186	0.053211665	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1085410_c0_g1	13.86186344	6.906225286	3.581560433	1.928272722	0.053821213	NA	up_regulated
stress	under	CvsDr	TRINITY_DN91_c5_g1	797.5433346	6.950507892	2.723640252	2.55191848	0.010713158	0.099944273	up_regulated
stress	under	CvsDr	TRINITY_DN1617615_c0_g1	14.34824462	6.956038248	3.577775649	1.94423545	0.051867065	NA	up_regulated
stress	under	CvsDr	TRINITY_DN3316_c0_g2	39.4541522	6.978397921	3.332945955	2.0937627	0.036281124	0.204115944	up_regulated
stress	under	CvsDr	TRINITY_DN29317_c0_g1	40.96900425	6.99090828	2.913714063	2.399311713	0.016425925	0.122281886	up_regulated
stress	under	CvsDr	TRINITY_DN10912_c0_g1	402.6072202	6.99116749	2.712700856	2.577198099	0.009960485	0.099501893	up_regulated
stress	under	CvsDr	TRINITY_DN6035_c1_g1	40.42691455	7.013822916	3.333081102	2.104306106	0.035351754	0.20354877	up_regulated
stress	under	CvsDr	TRINITY_DN2512_c0_g2	15.27081315	7.049570604	3.541046899	1.990815373	0.046501189	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1299382_c0_g1	15.56419755	7.073529051	3.569588283	1.98160922	0.047522994	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1360971_c0_g1	16.29376931	7.13968991	3.565412833	2.002486176	0.045232468	NA	up_regulated
stress	under	CvsDr	TRINITY_DN7122_c1_g1	17.26653166	7.223434991	3.560551883	2.028740271	0.042484752	NA	up_regulated
stress	under	CvsDr	TRINITY_DN22891_c0_g1	18.9773212	7.362738404	3.523347349	2.089699843	0.036644771	NA	up_regulated
stress	under	CvsDr	TRINITY_DN471033_c0_g1	19.21205635	7.377616957	3.552763251	2.076585586	0.037839824	NA	up_regulated
stress	under	CvsDr	TRINITY_DN11041_c0_g1	20.67119987	7.483322317	3.54822996	2.109029686	0.034942019	NA	up_regulated
stress	under	CvsDr	TRINITY_DN144174_c0_g1	20.91439046	7.500209599	3.54756284	2.114186538	0.034499338	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1351966_c0_g1	21.40077163	7.533404501	3.546295907	2.124302285	0.033644875	NA	up_regulated
stress	under	CvsDr	TRINITY_DN43266_c0_g1	115.0189012	7.557896659	2.839210436	2.661971287	0.00776845	0.092151384	up_regulated
stress	under	CvsDr	TRINITY_DN63509_c0_g1	22.61672457	7.613196809	3.54348411	2.148505982	0.031673582	NA	up_regulated
stress	under	CvsDr	TRINITY_DN10478_c0_g1	23.81658176	7.690393767	3.228656265	2.381917781	0.017222742	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1822818_c0_g1	25.53501161	7.788415301	3.538395605	2.201114903	0.02772789	0.166141913	up_regulated
stress	under	CvsDr	TRINITY_DN655801_c0_g1	27.99883261	7.923093135	2.981648472	2.657286132	0.007877255	0.092151384	up_regulated
stress	under	CvsDr	TRINITY_DN2123_c1_g1	387.0113714	7.950682715	2.448039759	3.247775158	0.001163111	0.031748634	up_regulated
stress	under	CvsDr	TRINITY_DN1266662_c0_g1	1697.689768	7.991704801	2.871640373	2.782975499	0.005386287	0.076888617	up_regulated
stress	under	CvsDr	TRINITY_DN39791_c3_g1	30.15563276	8.028524321	3.53359241	2.272057269	0.023083052	0.151894723	up_regulated
stress	under	CvsDr	TRINITY_DN7172_c0_g1	30.88520452	8.063033976	3.533088058	2.282149169	0.022480535	0.149921179	up_regulated
stress	under	CvsDr	TRINITY_DN171_c1_g1	31.37158569	8.085589138	3.532781979	2.288731427	0.022094962	0.149394375	up_regulated
stress	under	CvsDr	TRINITY_DN1352798_c0_g1	31.8759692	8.11035082	3.502720672	2.315443217	0.020588689	0.141046167	up_regulated
stress	under	CvsDr	TRINITY_DN21575_c0_g1	362.569356	8.141296413	2.793311536	2.914567999	0.003561812	0.064025743	up_regulated
stress	under	CvsDr	TRINITY_DN26933_c0_g1	33.31711038	8.172443418	3.531771757	2.31397836	0.020668909	0.141046167	up_regulated
stress	under	CvsDr	TRINITY_DN219267_c0_g1	35.01944449	8.244375086	3.531129181	2.334770172	0.01955543	0.139925748	up_regulated
stress	under	CvsDr	TRINITY_DN1500_c0_g1	103.1672834	8.327919895	2.982461342	2.792297683	0.005233518	0.076888617	up_regulated
stress	under	CvsDr	TRINITY_DN399689_c0_g2	200.7467884	8.367989439	3.259937413	2.566917207	0.010260711	0.099501893	up_regulated
stress	under	CvsDr	TRINITY_DN42725_c0_g1	40.91984883	8.470485744	3.500676634	2.419671003	0.015534555	0.118030586	up_regulated
stress	under	CvsDr	TRINITY_DN1798227_c0_g1	44.32983624	8.585909069	3.500829924	2.452535329	0.014185345	0.113781382	up_regulated
stress	under	CvsDr	TRINITY_DN61618_c0_g1	46.55374106	8.65649709	3.501093142	2.472512652	0.013416696	0.113624576	up_regulated
stress	under	CvsDr	TRINITY_DN834185_c0_g1	47.42216442	8.681980049	3.530524964	2.459118725	0.013927855	0.113781382	up_regulated
stress	under	CvsDr	TRINITY_DN25330_c0_g2	50.34045146	8.768168857	3.53098736	2.48320596	0.013020578	0.113624576	up_regulated
stress	under	CvsDr	TRINITY_DN17244_c0_g1	52.78067458	8.837536228	3.502306768	2.523347272	0.011624354	0.105767273	up_regulated
stress	under	CvsDr	TRINITY_DN76160_c0_g2	56.17702554	8.926486007	3.532259013	2.527132346	0.011499816	0.105767273	up_regulated
stress	under	CvsDr	TRINITY_DN50476_c0_g1	61.67629389	9.062162529	3.504773407	2.585662888	0.009719194	0.099501893	up_regulated
stress	under	CvsDr	TRINITY_DN5125_c0_g1	65.67932259	9.152856596	3.506036197	2.610599572	0.009038366	0.099501893	up_regulated
stress	under	CvsDr	TRINITY_DN223639_c0_g1	66.63422077	9.172843165	3.535204307	2.594713733	0.009466973	0.099501893	up_regulated
stress	under	CvsDr	TRINITY_DN10234_c0_g1	76.24768199	9.367289938	2.989170009	3.133742781	0.001725921	0.037147325	up_regulated
stress	under	CvsDr	TRINITY_DN39791_c4_g1	2287.765707	9.554982726	2.803735262	3.407947553	0.000654535	0.021156092	up_regulated
stress	under	CvsDr	TRINITY_DN301_c0_g1	102.2409728	9.79127343	3.05988482	3.199883004	0.001374834	0.033775085	up_regulated
stress	under	CvsDr	TRINITY_DN171_c3_g1	150.5258269	10.34916638	3.218548684	3.215476102	0.001302283	0.033775085	up_regulated
stress	under	CvsDr	TRINITY_DN171_c0_g1	194.3028682	10.71705211	3.115141354	3.440310052	0.000581048	0.021156092	up_regulated
stress	under	CvsDr	TRINITY_DN716478_c0_g1	196.2849363	10.7320508	3.20404761	3.349529128	0.00080949	0.024858102	up_regulated
stress	under	CvsDr	TRINITY_DN60_c7_g1	674.2086514	11.02473977	3.435493394	3.209070285	0.001331649	0.033775085	up_regulated
stress	under	CvsDr	TRINITY_DN976820_c0_g1	126.4660545	21.4769917	3.526127441	6.090815507	1.12E-09	4.87E-08	up_regulated
stress	under	CvsDr	TRINITY_DN1273607_c0_g1	94.60113825	21.83638285	3.544460074	6.160707807	7.24E-10	3.34E-08	up_regulated
stress	under	CvsDr	TRINITY_DN511505_c0_g1	98.88963468	22.48002484	3.517261622	6.391342828	1.64E-10	8.34E-09	up_regulated
stress	under	CvsDr	TRINITY_DN17244_c1_g1	99.92745694	22.49471155	3.517605127	6.394893894	1.61E-10	8.34E-09	up_regulated
stress	under	CvsDr	TRINITY_DN61290_c0_g1	113.8131946	22.67524922	3.550541308	6.386420338	1.70E-10	8.34E-09	up_regulated
stress	under	CvsDr	TRINITY_DN160_c2_g2	119.1633875	22.74064663	3.552161095	6.401918725	1.53E-10	8.34E-09	up_regulated
stress	under	CvsDr	TRINITY_DN161957_c0_g1	151.5077355	23.07479638	3.561221999	6.479460248	9.21E-11	7.54E-09	up_regulated
stress	under	CvsDr	TRINITY_DN1762827_c0_g1	162.2081214	23.16461142	3.563956141	6.499690371	8.05E-11	7.41E-09	up_regulated
stress	under	CvsDr	TRINITY_DN1174_c0_g1	178.0606466	23.29858713	3.539999259	6.581523166	4.66E-11	4.90E-09	up_regulated
stress	under	CvsDr	TRINITY_DN60096_c1_g1	330.3239971	24.14227824	3.568211896	6.765931773	1.32E-11	1.95E-09	up_regulated
stress	under	CvsDr	TRINITY_DN171_c0_g2	336.0893909	24.17620673	3.5956839	6.723674105	1.77E-11	2.18E-09	up_regulated
stress	under	CvsDxD	TRINITY_DN13622_c0_g1	46.25063	4.601254899	2.215498182	2.076848872	0.03781551	0.174901395	up_regulated
stress	under	CvsDxD	TRINITY_DN6035_c3_g1	40.32887462	4.926810991	2.53234308	1.945554309	0.0517083	0.224317828	up_regulated
stress	under	CvsDxD	TRINITY_DN43266_c0_g1	16.64858047	4.943878322	2.622437071	1.885222863	0.059399755	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN5340_c0_g1	252.7960716	5.02209796	2.239441522	2.242567136	0.024924741	0.131374158	up_regulated
stress	under	CvsDxD	TRINITY_DN24919_c0_g1	52.41290844	5.084154982	2.421862728	2.09927463	0.035792699	0.166666615	up_regulated
stress	under	CvsDxD	TRINITY_DN3456_c0_g1	1568.784974	5.32672457	2.399935582	2.219528145	0.026450815	0.136572574	up_regulated
stress	under	CvsDxD	TRINITY_DN39791_c4_g1	131.7361002	5.621973129	2.474040926	2.272384854	0.023063276	0.125935441	up_regulated
stress	under	CvsDxD	TRINITY_DN121_c0_g1	310.9847259	5.755440865	2.311696655	2.489704198	0.012784945	0.094211393	up_regulated
stress	under	CvsDxD	TRINITY_DN6035_c0_g2	127.2756634	5.833141207	2.272927971	2.566355503	0.010277344	0.086672264	up_regulated
stress	under	CvsDxD	TRINITY_DN129372_c0_g1	18.09127865	5.842821086	2.838470824	2.058439719	0.039547942	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN6750_c0_g1	20.70512814	6.042062875	2.844130175	2.124397445	0.033636924	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1113321_c0_g1	7.06214004	6.045643289	3.102398269	1.94869993	0.05133127	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1439974_c0_g1	7.168501889	6.066225651	3.130582501	1.937730646	0.052656093	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN45506_c1_g1	7.234827622	6.079340864	3.146459234	1.932121287	0.053344538	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN110134_c0_g1	21.73558377	6.094703252	3.222077772	1.891544427	0.058551706	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN446864_c0_g1	7.32744297	6.098041405	3.112838012	1.958997346	0.050113098	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN5340_c2_g1	21.96085058	6.109829821	3.221699115	1.896461961	0.057898985	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN160_c5_g2	20.03859265	6.139723159	3.218515265	1.907625925	0.056439582	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN78823_c0_g1	7.652198381	6.160356279	3.124169271	1.971838189	0.048628081	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1773020_c0_g1	223.7841104	6.182082296	3.101406722	1.993315566	0.046226903	0.205484948	up_regulated
stress	under	CvsDxD	TRINITY_DN159361_c0_g1	10.21467203	6.579264992	3.207872622	2.050974514	0.040269429	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1274526_c0_g1	10.4880516	6.615153742	3.130255417	2.113295199	0.034575509	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN156101_c0_g1	10.61949968	6.634479727	3.002961324	2.209312412	0.027152918	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1454_c0_g1	329.9058936	6.650185947	2.901935073	2.291638434	0.021926516	0.123275744	up_regulated
stress	under	CvsDxD	TRINITY_DN1078665_c0_g1	10.757821	6.653778121	3.487795603	1.907731667	0.056425907	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1266662_c0_g1	581.4068192	6.673090297	2.930547804	2.277079489	0.022781479	0.125935441	up_regulated
stress	under	CvsDxD	TRINITY_DN6843_c0_g2	10.94992495	6.679298814	3.485000315	1.916584853	0.055290694	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN296853_c0_g1	11.03807382	6.68907816	3.493820795	1.914545294	0.055550518	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN59871_c0_g1	11.1420289	6.70437581	3.48231651	1.925263195	0.05419644	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN947215_c0_g1	11.20268477	6.711340909	2.988780616	2.245511388	0.024735322	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN945474_c0_g1	282.8806446	6.718002292	2.885297142	2.328357171	0.019893146	0.11833696	up_regulated
stress	under	CvsDxD	TRINITY_DN972457_c0_g1	11.26334063	6.718240215	3.490741753	1.92458815	0.054280903	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN977329_c0_g1	11.28963025	6.72154647	3.070766634	2.188882214	0.028605402	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN174364_c1_g1	11.33413284	6.729024269	3.479738381	1.933773041	0.053141039	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN168500_c0_g1	11.48860745	6.746824559	3.487803055	1.934405255	0.053063321	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN29317_c0_g1	33.5841692	6.755383305	2.709827529	2.492920022	0.012669738	0.094211393	up_regulated
stress	under	CvsDxD	TRINITY_DN64994_c0_g1	11.71834074	6.777092498	3.474877819	1.950311019	0.05113906	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN267925_c0_g1	11.91044468	6.800539293	3.472585577	1.958350383	0.050188913	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN5665_c0_g1	305.6694255	6.812196895	2.843353826	2.395831582	0.016582708	0.104885627	up_regulated
stress	under	CvsDxD	TRINITY_DN1622692_c0_g1	12.16440789	6.829329015	3.479748963	1.962592442	0.04969355	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN237246_c0_g1	12.16440789	6.829329015	3.479748963	1.962592442	0.04969355	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN3479_c1_g1	12.3896747	6.855814445	3.477294488	1.97159443	0.048655924	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN111551_c1_g1	12.61494151	6.881822449	3.474944512	1.980412184	0.047657232	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN295380_c0_g1	12.61494151	6.881822449	3.474944512	1.980412184	0.047657232	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN876088_c0_g1	12.61494151	6.881822449	3.474944512	1.980412184	0.047657232	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN90847_c0_g2	12.84020832	6.907369935	3.472693336	1.989052665	0.04669539	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN660589_c0_g1	13.06547514	6.932472926	3.470535658	1.99752246	0.045768457	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN658105_c0_g1	13.29074195	6.957146626	3.468466537	2.005827806	0.044874623	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN26933_c0_g1	13.44727626	6.9755378	3.457024297	2.017786744	0.043613476	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1863013_c0_g1	13.6393802	6.995992086	3.455375598	2.024669067	0.042901357	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN20873_c0_g2	13.74127557	7.00526319	3.46457583	2.021968499	0.043179606	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1320548_c0_g1	14.40779599	7.075026901	3.449320795	2.051136244	0.040253681	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN78405_c0_g1	14.79887713	7.113754462	3.032505627	2.345833887	0.018984566	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1911001_c0_g1	15.02655072	7.134190277	3.124342259	2.283421496	0.022405552	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN138189_c0_g1	40.57464564	7.165890271	3.217490809	2.227167286	0.025936096	0.135762045	up_regulated
stress	under	CvsDxD	TRINITY_DN23303_c0_g1	190.9362017	7.170412868	2.585182484	2.773658306	0.005542986	0.063744344	up_regulated
stress	under	CvsDxD	TRINITY_DN1394900_c0_g1	76.29221585	7.18078669	2.793772741	2.570283039	0.010161545	0.086658573	up_regulated
stress	under	CvsDxD	TRINITY_DN112686_c0_g1	15.54341008	7.183127359	3.451764979	2.081001285	0.037433788	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN5125_c0_g1	16.71304335	7.289059407	3.435280957	2.121823367	0.033852574	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1128154_c0_g1	17.12027776	7.322585773	3.443321065	2.126605575	0.033452869	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN791028_c0_g1	17.32251805	7.340844368	3.157936302	2.32457012	0.020094958	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN6035_c1_g1	45.84684722	7.341119027	2.931013639	2.504634891	0.012257785	0.092115437	up_regulated
stress	under	CvsDxD	TRINITY_DN662825_c0_g1	17.7960782	7.378459531	3.440302074	2.144712695	0.031975825	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN71219_c0_g1	17.86566702	7.385236856	3.430003062	2.153128357	0.031308592	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN797637_c0_g1	18.02134502	7.396613488	3.439363961	2.15057597	0.031509684	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN112443_c0_g1	18.47187864	7.432250164	3.437582003	2.162057562	0.03061374	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1577083_c0_g1	54.42556123	7.448867607	2.86489731	2.600046983	0.0093211	0.083231938	up_regulated
stress	under	CvsDxD	TRINITY_DN2123_c1_g1	273.8101341	7.575479796	2.313176634	3.274924916	0.001056901	0.020054689	up_regulated
stress	under	CvsDxD	TRINITY_DN1452574_c0_g1	20.49927996	7.582543633	3.430897534	2.210075806	0.027099902	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN182167_c0_g1	20.49927996	7.582543633	3.430897534	2.210075806	0.027099902	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1585434_c0_g1	21.85088083	7.67469072	3.427423878	2.239200926	0.025142846	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN681921_c0_g1	24.32881577	7.829710265	3.42256078	2.287676032	0.022156394	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN93451_c0_g1	25.39329058	7.891916556	2.875938558	2.744118624	0.006067361	0.064459963	up_regulated
stress	under	CvsDxD	TRINITY_DN171_c1_g1	29.04085879	8.085998957	3.141658442	2.573799509	0.010058854	0.086658573	up_regulated
stress	under	CvsDxD	TRINITY_DN171_c0_g2	32.27346301	8.238136978	3.405029283	2.419402682	0.01554602	0.099995162	up_regulated
stress	under	CvsDxD	TRINITY_DN760950_c0_g1	32.88895465	8.264751283	3.414560818	2.420443426	0.015501591	0.099995162	up_regulated
stress	under	CvsDxD	TRINITY_DN161957_c0_g1	34.01528872	8.313341458	3.414113613	2.434992622	0.014892093	0.099995162	up_regulated
stress	under	CvsDxD	TRINITY_DN9571_c0_g1	34.48403532	8.333201675	2.874507638	2.899001403	0.003743532	0.049848087	up_regulated
stress	under	CvsDxD	TRINITY_DN134731_c0_g1	34.96291826	8.353588283	3.404021096	2.454035403	0.014126307	0.096593399	up_regulated
stress	under	CvsDxD	TRINITY_DN12342_c0_g1	36.49974984	8.415636033	3.403661512	2.472524369	0.013416257	0.094401759	up_regulated
stress	under	CvsDxD	TRINITY_DN1075806_c0_g1	36.71849047	8.423685762	3.41338749	2.467837533	0.0135932	0.094653567	up_regulated
stress	under	CvsDxD	TRINITY_DN1617615_c0_g1	37.26816562	8.44568704	3.403530968	2.481448566	0.013084959	0.094401759	up_regulated
stress	under	CvsDxD	TRINITY_DN15248_c0_g1	40.32275947	8.558797042	3.413009393	2.507698062	0.012152046	0.092115437	up_regulated
stress	under	CvsDxD	TRINITY_DN664347_c0_g1	42.83918007	8.646635385	3.403340858	2.540631617	0.011065244	0.088322486	up_regulated
stress	under	CvsDxD	TRINITY_DN1915286_c0_g1	43.47649485	8.667455732	3.413080668	2.539481652	0.011101687	0.088322486	up_regulated
stress	under	CvsDxD	TRINITY_DN1500_c0_g1	144.0831182	8.869964269	2.659444073	3.335270089	0.000852166	0.017020892	up_regulated
stress	under	CvsDxD	TRINITY_DN56714_c0_g1	50.14600329	8.873885046	3.014214392	2.944012566	0.003239868	0.044714478	up_regulated
stress	under	CvsDxD	TRINITY_DN4557_c0_g1	1782.612177	8.918451331	2.425257162	3.677321924	0.000235696	0.006389032	up_regulated
stress	under	CvsDxD	TRINITY_DN33894_c0_g1	152.6843194	9.081455219	2.887792613	3.144774032	0.001662152	0.026071669	up_regulated
stress	under	CvsDxD	TRINITY_DN509816_c0_g1	187.4656322	9.233403111	2.993771316	3.084204549	0.002040972	0.030981958	up_regulated
stress	under	CvsDxD	TRINITY_DN11435_c1_g1	67.12951018	9.294244378	3.418965694	2.718437449	0.006559106	0.067275151	up_regulated
stress	under	CvsDxD	TRINITY_DN42134_c0_g3	72.08538006	9.397013591	3.42064039	2.747150393	0.006011556	0.064459963	up_regulated
stress	under	CvsDxD	TRINITY_DN14462_c0_g1	75.61645008	9.465997548	3.010019192	3.144829632	0.001661836	0.026071669	up_regulated
stress	under	CvsDxD	TRINITY_DN1610597_c0_g1	87.19767288	9.671609546	2.989687904	3.23498969	0.001216471	0.021983373	up_regulated
stress	under	CvsDxD	TRINITY_DN16256_c0_g1	117.1249961	10.09733219	2.998691527	3.367246047	0.000759229	0.015574458	up_regulated
stress	under	CvsDxD	TRINITY_DN1735_c0_g1	167.2630849	10.61158679	3.093625568	3.430145814	0.000603257	0.013466825	up_regulated
stress	under	CvsDxD	TRINITY_DN160_c2_g2	176.1450632	10.68619689	2.802109509	3.81362572	0.000136943	0.003849615	up_regulated
stress	under	CvsDxD	TRINITY_DN1150_c0_g1	717.9519514	11.40882857	3.338571098	3.417278902	0.000632504	0.013681185	up_regulated
stress	under	CvsDxD	TRINITY_DN10234_c0_g1	312.7201387	11.51430566	2.981570251	3.861826049	0.000112543	0.003416796	up_regulated
stress	under	CvsDxD	TRINITY_DN171_c0_g1	416.4942689	11.92768012	2.802334379	4.256337219	2.08E-05	0.000788613	up_regulated
stress	under	CvsDxD	TRINITY_DN655801_c0_g1	463.1031625	12.08073142	3.149135413	3.836205761	0.00012495	0.003647571	up_regulated
stress	under	CvsDxD	TRINITY_DN471033_c0_g1	693.1848154	12.66259283	3.061588087	4.135955743	3.53E-05	0.001166485	up_regulated
stress	under	CvsDxD	TRINITY_DN7172_c0_g1	900.3113781	13.03977408	3.109594886	4.193399641	2.75E-05	0.000993223	up_regulated
stress	under	CvsDxD	TRINITY_DN38640_c0_g1	96.86472946	22.56738187	3.429201573	6.580943518	4.67E-11	2.13E-09	up_regulated
stress	under	CvsDxD	TRINITY_DN20873_c0_g1	123.6714802	22.90711802	3.437861926	6.663187327	2.68E-11	1.69E-09	up_regulated
stress	under	CvsDxD	TRINITY_DN20579_c0_g1	190.3504567	23.30799138	3.455597638	6.744995749	1.53E-11	1.06E-09	up_regulated
stress	under	CvsDxD	TRINITY_DN900_c0_g1	182.4661183	23.43513863	3.453749782	6.78541878	1.16E-11	8.79E-10	up_regulated
stress	under	CvsDxD	TRINITY_DN660427_c0_g1	1302.717978	26.18132279	3.548740015	7.377639017	1.61E-13	4.08E-11	up_regulated
stress	under	CvsDxD	TRINITY_DN443057_c0_g1	1705.495039	26.56289151	3.562224431	7.456827054	8.86E-14	3.36E-11	up_regulated
stress	under	CvsDxD	TRINITY_DN3456_c4_g1	2261.904066	26.7457582	3.576305587	7.478599786	7.51E-14	3.36E-11	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


HSP20_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

HSP20_reg

HSP20_reg_final <- HSP20_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

HSP20_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

HSP20_reg_final_plot <- HSP20_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                               panel.background = element_rect(fill = "white"), 
                                                                                               panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                               axis.title.x = element_text(color="black", vjust=1),
                                                                                               axis.title.y = element_text(color="black" , vjust=1)) 






HSP20_reg_final_plot


jpeg("HSP20_regulation_under.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(HSP20_reg_final_plot)
dev.off()







################ heat shock protein 70 inter


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	inter	CvsDr	TRINITY_DN8243_c0_g1	99.90989177	-23.25462779	3.587503851	-6.482119255	9.04E-11	2.98E-09	down_regulated
stress	inter	CvsDr	TRINITY_DN8629_c1_g3	47.96558963	-8.902775647	3.574519584	-2.490621589	0.012751985	0.096583235	down_regulated
stress	inter	CvsDr	TRINITY_DN5943_c0_g1	42.21856046	-8.718658554	3.574709539	-2.438983772	0.01472863	0.101663468	down_regulated
stress	inter	CvsDr	TRINITY_DN88882_c0_g1	34.70321462	-8.435858526	3.577002908	-2.358359426	0.018355911	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN13365_c0_g1	26.13128271	-8.026530768	3.586223189	-2.238157066	0.025210814	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN16803_c0_g1	20.99876044	-7.711116676	3.597587925	-2.143412986	0.032079951	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN25413_c0_g1	20.33564169	-7.664824647	3.599812503	-2.129228853	0.033235331	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN12117_c0_g1	19.07393615	-7.572439471	3.079449167	-2.459024021	0.01393153	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN38173_c0_g2	18.57423916	-7.534155577	3.227274536	-2.334525772	0.019568208	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN10478_c0_g1	18.56732502	-7.533584817	3.606840449	-2.088693671	0.036735307	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN20797_c1_g1	15.76525654	-7.297578314	3.097392374	-2.35603935	0.018470961	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN2592_c1_g1	15.47277085	-7.270560338	3.624462881	-2.005969043	0.044859551	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN28403_c1_g1	15.03069168	-7.228741806	3.627742244	-1.992628285	0.046302166	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN32949_c0_g1	14.51737928	-7.178528106	3.632490068	-1.97620034	0.048132091	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN172381_c0_g1	14.14653335	-7.14128249	3.635060917	-1.964556483	0.049465593	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN6475_c1_g1	13.04133543	-7.023930336	3.645911868	-1.92652225	0.054039196	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN59232_c0_g1	12.59925626	-6.974179395	3.650888153	-1.910269256	0.056098554	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN6475_c0_g2	12.50728061	-6.963511397	3.652606408	-1.906449975	0.05659185	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN65662_c0_g2	12.50728061	-6.963511397	3.652606408	-1.906449975	0.05659185	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN877364_c0_g1	11.93613751	-6.89618039	3.659164225	-1.884632655	0.05947945	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN14146_c0_g1	8.883068178	-6.469966119	3.215050241	-2.012399693	0.044177821	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN48529_c0_g1	8.864630473	-6.467004257	3.237696886	-1.997408801	0.045780792	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN78957_c1_g1	8.848497481	-6.464400188	3.315718737	-1.949622601	0.051221118	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN14896_c0_g1	38.03316977	-6.180127388	3.250514119	-1.901276894	0.057265754	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN47642_c0_g2	21.23621525	-5.245806282	2.56037161	-2.048845669	0.040477208	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN8243_c0_g1	117.5906749	-23.86815768	3.501051039	-6.81742637	9.27E-12	5.31E-10	down_regulated
stress	inter	CvsDi	TRINITY_DN2818_c0_g1	91.05472613	-23.51945331	3.494439325	-6.73053704	1.69E-11	7.27E-10	down_regulated
stress	inter	CvsDi	TRINITY_DN207_c4_g2	72.00873166	-9.779970933	2.92629357	-3.342101775	0.000831466	0.020405157	down_regulated
stress	inter	CvsDi	TRINITY_DN1080_c3_g1	67.94915238	-9.696456247	2.917765425	-3.323247361	0.00088976	0.020405157	down_regulated
stress	inter	CvsDi	TRINITY_DN8629_c1_g3	56.4539302	-9.428786505	3.489053535	-2.702390895	0.006884277	0.089270477	down_regulated
stress	inter	CvsDi	TRINITY_DN5943_c0_g1	49.68986483	-9.244646655	3.489573834	-2.649219387	0.008067794	0.09096593	down_regulated
stress	inter	CvsDi	TRINITY_DN88882_c0_g1	40.84454858	-8.961806841	3.492474702	-2.566033431	0.010286891	0.098296961	down_regulated
stress	inter	CvsDi	TRINITY_DN7446_c0_g1	27.40605832	-8.386031658	3.095534219	-2.709074126	0.006747126	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN15115_c0_g1	23.67422879	-8.174849214	3.518604141	-2.32332166	0.020161879	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN10478_c0_g1	21.85313427	-8.059346186	3.525348872	-2.286113085	0.022247644	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN2592_c1_g1	18.21094523	-7.79624559	3.544275961	-2.199672282	0.027830155	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN28403_c1_g1	17.6906325	-7.754413893	3.547784358	-2.185706094	0.028837115	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN172381_c0_g1	16.65000706	-7.666926099	3.555602516	-2.156294486	0.031060678	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN32949_c0_g1	15.06290805	-7.5234216	3.562880173	-2.111612301	0.034719717	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN59232_c0_g1	14.82891254	-7.499764643	3.572464997	-2.099324877	0.035788272	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN877364_c0_g1	14.04844346	-7.421736569	3.581262634	-2.072379863	0.03823003	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN58978_c0_g1	13.7882871	-7.39476034	3.584449172	-2.063011633	0.039111523	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN558662_c0_g1	13.52813074	-7.367270142	3.58777515	-2.053436973	0.040030217	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1399597_c0_g1	13.26797438	-7.339245993	3.591248907	-2.043647261	0.040988407	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN23959_c0_g1	13.26797438	-7.339245993	3.591248907	-2.043647261	0.040988407	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN24308_c1_g1	13.10622358	-7.322044049	3.084914704	-2.373499675	0.017620409	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN19441_c0_g1	13.00781802	-7.310666733	3.594879441	-2.033633353	0.041988576	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN65662_c0_g2	12.97727463	-7.308485624	3.588169314	-2.036828529	0.041667228	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN4770_c0_g1	33.64809569	-7.245471593	2.800857071	-2.586876592	0.009685027	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN6523_c2_g1	11.70703622	-7.158607521	3.615753982	-1.979838108	0.047721723	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN42452_c0_g1	11.44687986	-7.126173368	3.620560567	-1.96825139	0.04903912	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN17675_c0_g1	11.3551153	-7.115916174	3.615177762	-1.968344751	0.049028384	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN93428_c0_g1	11.3551153	-7.115916174	3.615177762	-1.968344751	0.049028384	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN24656_c0_g1	11.12337825	-7.086181154	3.619747842	-1.957644969	0.050271687	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN15549_c0_g1	10.65990416	-7.0248065	3.629538088	-1.935454686	0.052934524	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN37774_c1_g2	10.66641078	-7.024252863	3.636521922	-1.931585458	0.053410692	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN9948_c0_g1	9.94716286	-6.923576178	3.202685737	-2.161803169	0.030633351	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN37144_c0_g2	9.464034532	-6.852649243	3.171224488	-2.160884311	0.030704277	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN17071_c0_g1	9.370011513	-6.837431552	3.201504504	-2.135693248	0.032704425	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN22865_c3_g1	9.118620231	-6.799238706	3.19228442	-2.129897531	0.033180074	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN16075_c0_g1	8.736021538	-6.736604898	3.201912592	-2.103931542	0.035384419	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN65565_c0_g1	7.922750604	-6.595124766	3.282152614	-2.009390038	0.044495786	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN180311_c0_g1	7.907478909	-6.594266776	3.392523411	-1.94376456	0.051923849	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN9990_c0_g1	7.89871383	-6.591461328	3.229257506	-2.041169314	0.041234001	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN4330_c2_g1	7.776271498	-6.567987924	3.404542396	-1.929183767	0.053708053	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN11851_c1_g1	7.610138157	-6.537886518	3.241635334	-2.01684824	0.043711353	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN7923_c0_g1	20.98888743	-6.511046489	3.021631525	-2.154811543	0.031176585	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1446165_c0_g1	7.094207975	-6.437487936	3.309244944	-1.945304154	0.051738382	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN88160_c0_g1	6.919309555	-6.401282426	3.298633246	-1.940586282	0.052308479	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN6305_c6_g1	6.886507702	-6.393688397	3.280209926	-1.949170492	0.051275068	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN30437_c0_g2	6.825286536	-6.380151945	3.30911348	-1.928054744	0.053848317	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN395806_c0_g1	6.744411135	-6.364126059	3.296256094	-1.930713475	0.053518496	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN9917_c4_g1	6.536710862	-6.317933835	3.31784549	-1.904227865	0.056880514	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN841_c2_g1	14.09918427	-5.978191464	2.896709414	-2.06378708	0.039037909	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1318473_c0_g1	11.05818335	-5.621507362	2.96573262	-1.895486911	0.058027924	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN2210_c0_g1	38.60664632	-5.432816224	2.441005545	-2.225646818	0.026037848	0.203568627	down_regulated
stress	inter	CvsDi	TRINITY_DN38173_c0_g2	22.4468024	-5.00677346	2.606553009	-1.920840836	0.054751777	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN8243_c0_g1	99.88184688	-23.16095963	3.915908503	-5.91458141	3.33E-09	5.35E-08	down_regulated
stress	inter	CvsDxD	TRINITY_DN2818_c0_g1	77.34213807	-9.370041532	3.908189588	-2.39754017	0.016505571	0.082986683	down_regulated
stress	inter	CvsDxD	TRINITY_DN8629_c1_g3	47.9521256	-8.680338578	3.898544065	-2.226559052	0.025976758	0.111273576	down_regulated
stress	inter	CvsDxD	TRINITY_DN5943_c0_g1	42.20670963	-8.496199378	3.897392515	-2.179970158	0.029259674	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN6178_c0_g1	30.58148195	-8.031520293	3.308784453	-2.427332577	0.015210305	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN207_c4_g2	60.95629661	-7.557010365	3.10436041	-2.434321202	0.014919749	0.082986683	down_regulated
stress	inter	CvsDxD	TRINITY_DN16803_c0_g1	20.99286605	-7.48847273	3.907860021	-1.916259203	0.055332111	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN25413_c0_g1	20.32993344	-7.442169362	3.909246285	-1.903735099	0.056944693	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN38173_c0_g2	18.46539029	-7.303297177	3.498052273	-2.087818193	0.036814238	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN12117_c0_g1	18.10132162	-7.274820726	3.360561556	-2.164763419	0.030405808	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN841_c2_g1	11.60095929	-6.633027146	3.408361544	-1.946104326	0.051642209	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN224_c0_g2	142.6540655	5.58910087	2.805185514	1.992417558	0.046325263	0.278937219	up_regulated
stress	inter	CvsDr	TRINITY_DN229_c0_g1	47.78126838	6.137078115	2.88256092	2.129036744	0.033251221	0.213865805	up_regulated
stress	inter	CvsDr	TRINITY_DN20581_c0_g1	6.481262051	6.309210384	3.314842062	1.903321566	0.056998598	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN183557_c0_g1	6.619392778	6.339050865	3.31066009	1.914739264	0.055525764	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN196971_c0_g1	6.633327974	6.34315326	3.318822309	1.911266308	0.055970367	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN34075_c0_g1	6.85445926	6.390617396	3.317039713	1.926602618	0.054029172	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN399205_c0_g1	7.441514849	6.507137396	3.346183008	1.944644803	0.051817743	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN40231_c0_g1	7.759581891	6.569119045	3.260360046	2.014844665	0.043920927	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN1803_c5_g1	8.056440831	6.621671187	3.356621982	1.972718769	0.048527612	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN1102212_c0_g1	8.312104799	6.666830378	3.321695702	2.007056328	0.044743666	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN15382_c0_g1	8.50597631	6.702373388	3.337020948	2.008490055	0.044591242	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN20114_c0_g1	8.567768767	6.71062281	3.295016896	2.03659739	0.041690405	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN11042_c0_g2	9.438358716	6.850293172	3.276243952	2.09089838	0.036537174	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN17350_c0_g1	9.590424639	6.873604624	3.228938772	2.128750375	0.033274919	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN11209_c0_g1	9.624957321	6.878726975	3.236195722	2.125559628	0.033539944	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN79650_c0_g2	9.970894755	6.931349067	3.676552445	1.88528497	0.059391374	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN714002_c0_g1	10.22655872	6.967855101	3.671557004	1.897792978	0.057723359	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN6589_c3_g1	10.44707939	6.996883684	3.675237011	1.903791147	0.05693739	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN666_c20_g1	10.48222269	7.003459981	3.666832489	1.909948164	0.056139888	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN18630_c0_g1	10.73727604	7.036432374	3.670142343	1.917209665	0.055211301	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN85950_c0_g1	11.24860398	7.103467207	3.297213146	2.154385201	0.031209976	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN6305_c4_g1	12.0162065	7.200397303	3.643194922	1.976396393	0.0481099	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN3218_c0_g1	12.18825929	7.219385174	3.64874333	1.978594963	0.047861627	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN44546_c0_g1	12.18825929	7.219385174	3.64874333	1.978594963	0.047861627	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN820163_c0_g1	12.18825929	7.219385174	3.64874333	1.978594963	0.047861627	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN224_c3_g1	12.40272829	7.245103634	3.143656994	2.304673712	0.021184854	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN18926_c0_g5	12.87286125	7.299563374	3.162142639	2.308423183	0.020975611	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN224_c4_g1	13.13518751	7.328007186	3.129660984	2.341469963	0.019207972	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN202324_c0_g1	14.31718221	7.453049098	3.618477767	2.059719467	0.039425367	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN18354_c4_g1	14.62736573	7.482628059	3.201271818	2.337392288	0.019418796	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN4938_c1_g2	15.71908697	7.5865533	3.171558963	2.392058098	0.01675419	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN34108_c2_g1	17.12160234	7.70990927	3.606615662	2.137713023	0.032540042	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN18420_c0_g1	18.13759592	7.793347081	3.092097735	2.520407746	0.011721896	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN746995_c0_g1	18.61439117	7.830838428	3.083528967	2.539570249	0.011098875	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN21669_c2_g1	18.66346967	7.83537278	3.591159791	2.18185022	0.029120589	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN18313_c0_g2	19.15297889	7.871713928	3.596804466	2.188529847	0.02863103	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN9777_c1_g1	19.17479761	7.874353634	3.588964294	2.194046245	0.028232085	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN20849_c1_g1	20.02356884	7.935864374	3.593393892	2.208459359	0.027212268	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN17299_c0_g1	25.01931496	8.25732922	3.092638277	2.669995156	0.007585234	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN4938_c1_g1	27.27848508	8.382046629	3.576181188	2.343854013	0.01908564	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN1116925_c0_g1	31.44666807	8.587852814	3.563872846	2.40969675	0.015965785	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN102918_c0_g1	31.70233204	8.599532033	3.563662762	2.413116113	0.01581678	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN3947_c0_g1	86.82675208	8.602999819	2.901923633	2.964585188	0.003030913	0.042966542	up_regulated
stress	inter	CvsDr	TRINITY_DN3169_c0_g1	33.73248736	8.688638044	3.026826596	2.870543709	0.004097665	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN1192_c0_g1	1514.573944	14.17736886	3.299067178	4.297387139	1.73E-05	0.000444627	up_regulated
stress	inter	CvsDr	TRINITY_DN395_c0_g2	385.5412639	24.83821047	3.628362448	6.84557037	7.62E-12	5.39E-10	up_regulated
stress	inter	CvsDi	TRINITY_DN11951_c0_g1	27.77672607	5.12598658	2.695583898	1.901623831	0.05722035	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN63351_c0_g2	58.76225988	6.274202424	2.911658434	2.154855237	0.031173164	0.228161032	up_regulated
stress	inter	CvsDi	TRINITY_DN106330_c0_g1	7.063929126	6.288558387	3.293497926	1.909385865	0.056212333	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN79949_c0_g2	7.473422195	6.370619593	3.252310339	1.958798186	0.050136426	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN80224_c0_g1	7.520108885	6.380052256	3.310824485	1.927028233	0.053976112	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN40231_c0_g1	8.178309337	6.501032826	3.271930926	1.986910168	0.046932353	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN13634_c0_g1	8.318153183	6.52465938	3.213468664	2.030410146	0.042314866	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN96639_c0_g1	10.11145946	6.80706225	3.20796807	2.121923318	0.033844178	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN68663_c1_g1	10.65032546	6.881133937	3.148273718	2.185684777	0.028838675	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN8580_c0_g2	10.72799246	6.891369621	3.639907807	1.893281365	0.058320462	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN936997_c0_g1	11.89953028	7.041886722	3.611426314	1.949890738	0.051189143	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN1380989_c0_g1	12.04962882	7.05920706	3.122726817	2.260590655	0.023784617	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN818246_c0_g1	12.11202189	7.067414544	3.607811021	1.958920382	0.050122112	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN15382_c0_g1	12.96198834	7.165234615	3.594657352	1.993301145	0.046228481	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN20121_c0_g3	13.06561642	7.176792423	3.173965986	2.261143458	0.023750375	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN131890_c0_g2	68.51773136	7.177157238	2.740000156	2.619400303	0.008808452	0.094690855	up_regulated
stress	inter	CvsDi	TRINITY_DN17179_c3_g1	13.18997011	7.190349116	3.119486131	2.30497871	0.021167766	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN14628_c3_g1	13.34006865	7.205870567	3.136278681	2.297586184	0.021585353	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN1803_c5_g1	14.06044595	7.28174256	3.13700515	2.321240232	0.020273882	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN6475_c4_g1	14.22625087	7.298655568	3.58404972	2.036426986	0.041707498	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN43977_c1_g1	15.7243793	7.443879528	3.562805256	2.089331017	0.036677936	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN185946_c0_g1	15.96261576	7.465266593	3.0420004	2.454064961	0.014125146	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN59905_c0_g1	16.99932897	7.556328168	3.552082384	2.127295302	0.033395555	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN8023_c0_g3	19.19122495	7.73062249	3.101508007	2.492536686	0.012683423	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN307098_c0_g1	23.79906056	8.041658236	3.517529367	2.286166624	0.022244513	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN1101_c2_g2	67.6199974	8.056286498	3.29423244	2.445573178	0.01446221	0.124375003	up_regulated
stress	inter	CvsDi	TRINITY_DN28127_c0_g1	24.23910149	8.067585826	3.057675151	2.638470546	0.008328093	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN1192666_c0_g1	26.77394313	8.211554844	3.509253263	2.339972133	0.019285179	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN22996_c0_g1	27.47861404	8.24898057	2.978798017	2.769231255	0.005618874	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN6615_c0_g1	29.53633408	8.353194236	3.503621736	2.384159839	0.017118165	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN14659_c1_g1	31.25110846	8.434208599	3.506516893	2.405295299	0.016159402	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN17179_c0_g2	36.97354051	8.677152906	3.494507707	2.483083065	0.013025071	0.114887807	up_regulated
stress	inter	CvsDi	TRINITY_DN2449_c3_g1	59.710143	9.368565016	3.488632595	2.685454762	0.007243118	0.089270477	up_regulated
stress	inter	CvsDi	TRINITY_DN5785_c0_g1	110.2831467	21.47742964	3.498804541	6.138505134	8.33E-10	2.20E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN7876_c0_g5	87.53804376	5.038104692	2.658662242	1.894977336	0.058095404	0.213761296	up_regulated
stress	inter	CvsDxD	TRINITY_DN1357_c1_g3	157.7348168	5.676147821	2.767009804	2.051365273	0.040231388	0.154282645	up_regulated
stress	inter	CvsDxD	TRINITY_DN9949_c4_g1	143.2273136	6.01829593	2.738605045	2.197577172	0.027979251	0.116377466	up_regulated
stress	inter	CvsDxD	TRINITY_DN1357_c0_g2	42.37997692	6.055449006	2.867697193	2.111606839	0.034720185	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN41803_c0_g1	7.282562446	6.598904422	3.505368143	1.882513948	0.059766268	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN892749_c0_g1	7.714716946	6.681705528	3.506267873	1.905646051	0.056696143	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN47272_c0_g1	8.041295643	6.741515667	3.498650717	1.926890168	0.053993319	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN28127_c0_g1	8.121530797	6.756511226	3.511720575	1.923988849	0.054355981	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN224_c4_g1	8.385470306	6.801931873	3.499050738	1.943936336	0.051903129	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN11651_c0_g2	8.897859961	6.88782662	3.458092428	1.991799457	0.046393066	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN992_c5_g1	9.347610429	6.958678557	3.473613759	2.003296578	0.045145465	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN8023_c0_g3	10.34494249	7.104908333	3.46833038	2.048509673	0.040510085	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN271436_c0_g1	11.92504807	7.310061659	3.429364412	2.131608304	0.03303906	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN8596_c0_g1	247.4606013	7.317205265	3.541643337	2.066048037	0.038823944	0.152636602	up_regulated
stress	inter	CvsDxD	TRINITY_DN841_c6_g2	12.50007691	7.378498643	3.428014567	2.152411695	0.031364943	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN307098_c0_g1	12.6682919	7.397813	3.913937226	1.890120503	0.058741846	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN44314_c0_g1	12.73656917	7.40494191	3.91659018	1.890660388	0.058669693	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN42610_c0_g1	12.99276399	7.433685011	3.470683625	2.141850372	0.032205525	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN34962_c1_g1	13.28625736	7.466511344	3.909091597	1.91003745	0.056128392	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN44854_c0_g2	13.90422282	7.532086399	3.904784954	1.928937569	0.053738613	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN20581_c0_g1	14.04288395	7.545830329	3.907099738	1.93131244	0.053444426	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN36117_c0_g1	14.04288395	7.545830329	3.907099738	1.93131244	0.053444426	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1149787_c0_g1	172.0111471	7.60539502	3.525347008	2.157346498	0.030978677	0.127012574	up_regulated
stress	inter	CvsDxD	TRINITY_DN983471_c0_g1	14.64325344	7.606226358	3.481976086	2.184456805	0.028928698	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1192666_c0_g1	15.44913646	7.684060541	3.895928655	1.972330918	0.048571842	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN30145_c0_g1	16.00235613	7.734306381	3.89651079	1.984931339	0.047152111	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN22875_c0_g1	16.63791756	7.790482002	3.554908889	2.191471637	0.02841768	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN7446_c2_g1	16.68506738	7.795072115	3.890383597	2.003676995	0.045104672	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN32146_c0_g1	16.98209222	7.820049255	3.892422051	2.009044536	0.044532411	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN3831_c2_g1	403.3892332	7.832946415	3.183035862	2.460841397	0.013861163	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN224_c3_g1	17.60990896	7.87259787	3.358736514	2.343916481	0.019082444	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN45663_c0_g1	17.61201557	7.873061508	3.886920616	2.025526705	0.042813309	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN148007_c1_g1	17.96182831	7.900980588	3.888948925	2.031649359	0.042189166	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN942006_c0_g1	18.55655972	7.948484284	3.546775833	2.241045011	0.02502316	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN61141_c0_g1	18.9415644	7.97761216	3.885987764	2.052917468	0.040080583	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN10259_c2_g1	19.92130049	8.050377888	3.883455985	2.07299321	0.038172911	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN164360_c0_g1	20.0838774	8.06250973	3.879875755	2.078032968	0.037706323	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN209912_c0_g1	21.31980832	8.148653761	3.877264205	2.101650372	0.035583914	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN17179_c0_g2	22.24675651	8.210045739	3.875612181	2.118386814	0.034142322	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN59130_c0_g1	23.82264888	8.308418099	3.543429074	2.344739495	0.019040378	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN398_c1_g1	66.46661602	8.370591784	3.34904244	2.499398539	0.012440432	0.079342309	up_regulated
stress	inter	CvsDxD	TRINITY_DN28414_c0_g4	27.10603181	8.494726863	3.873185145	2.193214769	0.028291909	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN31840_c1_g2	27.43261051	8.512006382	3.872945199	2.197812245	0.027962489	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN3169_c0_g1	27.7064013	8.526332981	3.460771995	2.463708384	0.013750794	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN15329_c0_g3	28.56296562	8.570288971	3.380521282	2.535197461	0.011238396	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1079020_c0_g1	29.39208269	8.611550479	3.87176674	2.22419145	0.026135566	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN3102_c0_g1	29.66234201	8.62503246	3.868469686	2.229572198	0.025775857	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN90977_c0_g1	30.04524008	8.643261981	3.871461809	2.232557728	0.025578124	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN15455_c3_g1	34.61734183	8.847635244	3.870252663	2.286061406	0.022250666	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN20938_c0_g1	35.59707792	8.88790176	3.870159144	2.296521003	0.02164611	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN533526_c0_g1	35.92365662	8.901077995	3.870138273	2.299937978	0.021451734	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1098601_c0_g1	42.12865185	9.130958974	3.870496846	2.359118051	0.018318427	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN28537_c0_g1	43.76154534	9.185823563	3.870770751	2.373125187	0.017638285	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN988439_c0_g1	45.06786012	9.228260895	3.871028621	2.383929905	0.017128865	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN65253_c1_g1	50.05520214	9.379852401	3.869143589	2.424270949	0.015339156	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN3738_c1_g1	52.8360467	9.457849343	3.869958359	2.443915015	0.014528849	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN666_c1_g1	53.1971356	9.467532033	3.498386878	2.706256444	0.006804648	0.054248163	up_regulated
stress	inter	CvsDxD	TRINITY_DN666_c0_g1	1800.180277	9.671407969	3.640375381	2.656706234	0.007890817	0.059596434	up_regulated
stress	inter	CvsDxD	TRINITY_DN11297_c0_g1	63.33867116	9.719279819	3.553605112	2.735047793	0.006237122	0.0511444	up_regulated
stress	inter	CvsDxD	TRINITY_DN367032_c0_g1	64.00942452	9.734465252	3.876791114	2.510959442	0.012040353	0.078535939	up_regulated
stress	inter	CvsDxD	TRINITY_DN13651_c1_g1	75.67827776	9.976087667	3.447237246	2.893937073	0.003804444	0.036296737	up_regulated
stress	inter	CvsDxD	TRINITY_DN11209_c0_g1	94.03706864	10.2894403	3.56716811	2.88448427	0.003920553	0.036296737	up_regulated
stress	inter	CvsDxD	TRINITY_DN1873_c0_g1	858.8839821	11.95212003	3.734107729	3.20079679	0.001370481	0.014567711	up_regulated
stress	inter	CvsDxD	TRINITY_DN35341_c0_g1	81.64467414	22.79291067	3.883136163	5.869717082	4.37E-09	5.57E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN20125_c0_g1	97.94752517	23.04241921	3.88579497	5.929911224	3.03E-09	5.35E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN36193_c0_g1	121.1212299	23.17432863	3.893275901	5.952398243	2.64E-09	5.20E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN1237_c5_g1	271.595819	24.13531773	3.926897576	6.146154123	7.94E-10	2.85E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN12642_c2_g1	258.6185444	24.28300702	3.924702412	6.187222488	6.12E-10	2.85E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN19942_c0_g1	245.6412698	24.32671593	3.922409828	6.201982199	5.58E-10	2.85E-08	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


HSP70_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

HSP70_reg

HSP70_reg_final <- HSP70_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

HSP70_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

HSP70_reg_final_plot <- HSP70_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                               panel.background = element_rect(fill = "white"), 
                                                                                               panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                               axis.title.x = element_text(color="black", vjust=1),
                                                                                               axis.title.y = element_text(color="black" , vjust=1)) 






HSP70_reg_final_plot


jpeg("HSP70_regulation_inter.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(HSP70_reg_final_plot)
dev.off()




################ heat shock protein 70 under


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	under	CvsDi	TRINITY_DN6578_c0_g1	295.3610632	-25.08906816	3.281576016	-7.645432573	2.08E-14	6.75E-12	down_regulated
stress	under	CvsDi	TRINITY_DN1426_c0_g1	196.1155228	-24.50497665	3.263376263	-7.509087114	5.95E-14	8.52E-12	down_regulated
stress	under	CvsDi	TRINITY_DN1044_c3_g1	154.4112797	-24.36103179	3.253445466	-7.487763986	7.01E-14	8.52E-12	down_regulated
stress	under	CvsDi	TRINITY_DN1795_c4_g1	85.52009338	-23.55787499	3.23423656	-7.283905972	3.24E-13	2.63E-11	down_regulated
stress	under	CvsDi	TRINITY_DN1805_c0_g1	1065.657936	-10.75932991	2.175250649	-4.946248338	7.57E-07	4.33E-05	down_regulated
stress	under	CvsDi	TRINITY_DN275587_c0_g1	63.61216822	-9.728653339	3.228827943	-3.013060315	0.002586275	0.068643986	down_regulated
stress	under	CvsDi	TRINITY_DN280_c1_g1	61.36763843	-9.67631388	2.683950367	-3.605250678	0.000311852	0.015970093	down_regulated
stress	under	CvsDi	TRINITY_DN6765_c0_g1	53.054132	-9.466902919	3.227655178	-2.933058954	0.003356402	0.077756635	down_regulated
stress	under	CvsDi	TRINITY_DN2270_c2_g1	46.41820171	-9.273310454	2.712917913	-3.418205325	0.000630355	0.027878898	down_regulated
stress	under	CvsDi	TRINITY_DN36572_c0_g1	24.76898161	-8.368786305	2.913021853	-2.872888267	0.004067379	0.084203407	down_regulated
stress	under	CvsDi	TRINITY_DN226469_c0_g1	20.58817063	-8.102021609	3.263177458	-2.482862704	0.013033131	0.144015439	down_regulated
stress	under	CvsDi	TRINITY_DN106737_c0_g1	20.13411795	-8.070129778	3.010524464	-2.680639162	0.00734817	0.112025547	down_regulated
stress	under	CvsDi	TRINITY_DN1109470_c0_g1	18.47656338	-7.946034937	3.273884916	-2.427096596	0.015220203	0.151114871	down_regulated
stress	under	CvsDi	TRINITY_DN19203_c0_g1	12.22512066	-7.348692277	2.875822404	-2.555335916	0.010608536	NA	down_regulated
stress	under	CvsDi	TRINITY_DN550370_c0_g1	11.08593803	-7.209856737	3.353919263	-2.149681066	0.031580451	NA	down_regulated
stress	under	CvsDi	TRINITY_DN3496_c0_g1	8.95241503	-6.899131034	2.960322829	-2.330533334	0.01977798	NA	down_regulated
stress	under	CvsDi	TRINITY_DN88874_c0_g1	8.297302701	-6.786630044	3.10383374	-2.186531436	0.028776747	NA	down_regulated
stress	under	CvsDi	TRINITY_DN26615_c1_g2	7.918527165	-6.725105306	3.441846066	-1.953923905	0.050710217	NA	down_regulated
stress	under	CvsDi	TRINITY_DN109233_c0_g1	7.928057035	-6.721940255	3.028522172	-2.219544673	0.026449692	NA	down_regulated
stress	under	CvsDi	TRINITY_DN40491_c4_g1	7.76940089	-6.691977791	3.463946586	-1.931894048	0.053372585	NA	down_regulated
stress	under	CvsDi	TRINITY_DN976240_c0_g1	7.390625354	-6.625721743	3.464289329	-1.912577477	0.055802165	NA	down_regulated
stress	under	CvsDi	TRINITY_DN18694_c1_g1	7.210053452	-6.583938662	3.112884961	-2.115060063	0.034424828	NA	down_regulated
stress	under	CvsDi	TRINITY_DN115315_c2_g2	6.503007747	-6.436578635	3.085324858	-2.086191546	0.036961276	NA	down_regulated
stress	under	CvsDi	TRINITY_DN7687_c1_g2	6.196653336	-6.365213545	3.162146078	-2.012941018	0.044120835	NA	down_regulated
stress	under	CvsDi	TRINITY_DN60857_c0_g1	16.4015688	-6.3000133	2.708755657	-2.325796084	0.020029432	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1554397_c0_g1	5.795962042	-6.273010345	3.127042472	-2.006052173	0.044850682	NA	down_regulated
stress	under	CvsDi	TRINITY_DN19871_c1_g1	5.690667282	-6.249216814	3.206770417	-1.948757161	0.051324432	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1323639_c0_g1	14.45345814	-6.136110639	3.126164107	-1.962824225	0.049666602	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1164312_c0_g1	22.04419273	-5.752509722	2.948879539	-1.950744222	0.05108748	0.292598074	down_regulated
stress	under	CvsDi	TRINITY_DN59636_c0_g1	9.49786976	-5.498880175	2.79030782	-1.970707366	0.048757358	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1749_c3_g1	16.48201297	-5.278292852	2.693544481	-1.959608571	0.050041559	NA	down_regulated
stress	under	CvsDi	TRINITY_DN68707_c0_g1	17.20803961	-4.739635804	2.467397952	-1.92090449	0.05474375	0.304375248	down_regulated
stress	under	CvsDi	TRINITY_DN44626_c0_g1	29.87483184	-4.549387556	2.110611811	-2.155482847	0.031124069	0.208004966	down_regulated
stress	under	CvsDr	TRINITY_DN1044_c8_g1	86.24769838	-10.26989645	2.940142039	-3.492993303	0.000477638	0.019556643	down_regulated
stress	under	CvsDr	TRINITY_DN3050_c0_g1	76.28084928	-10.09275136	2.95268052	-3.418165727	0.000630447	0.021156092	down_regulated
stress	under	CvsDr	TRINITY_DN6765_c0_g1	60.39401409	-9.755826675	3.487118019	-2.797676082	0.00514717	0.076888617	down_regulated
stress	under	CvsDr	TRINITY_DN61496_c0_g1	38.05370323	-9.089172374	2.98766434	-3.042233444	0.002348297	0.045544602	down_regulated
stress	under	CvsDr	TRINITY_DN36572_c0_g1	28.02470252	-8.648383567	3.123990896	-2.768376687	0.00563363	0.076888617	down_regulated
stress	under	CvsDr	TRINITY_DN45253_c0_g1	26.14069267	-8.547912477	3.490449457	-2.448943204	0.014327604	0.113781382	down_regulated
stress	under	CvsDr	TRINITY_DN226469_c0_g1	23.43648308	-8.39040793	3.494698999	-2.400895738	0.016354996	NA	down_regulated
stress	under	CvsDr	TRINITY_DN220299_c0_g1	23.21449712	-8.376671315	3.018730953	-2.774898275	0.005521898	NA	down_regulated
stress	under	CvsDr	TRINITY_DN74260_c0_g1	18.68313502	-8.063582907	3.16607841	-2.546867722	0.010869463	NA	down_regulated
stress	under	CvsDr	TRINITY_DN53762_c0_g1	114.639624	-7.957382441	2.612429612	-3.045970082	0.002319309	0.045544602	down_regulated
stress	under	CvsDr	TRINITY_DN6578_c0_g1	337.6825354	-7.866943449	3.204259134	-2.455152071	0.0140825	0.113781382	down_regulated
stress	under	CvsDr	TRINITY_DN1323639_c0_g1	16.22525752	-7.86004349	3.518835788	-2.233705681	0.025502445	NA	down_regulated
stress	under	CvsDr	TRINITY_DN29487_c0_g1	15.35092214	-7.78026696	3.232241966	-2.407080609	0.01608062	NA	down_regulated
stress	under	CvsDr	TRINITY_DN86373_c0_g1	14.57669297	-7.70446107	3.108403265	-2.478591229	0.013190237	NA	down_regulated
stress	under	CvsDr	TRINITY_DN861150_c0_g1	14.1219834	-7.659813798	3.532744864	-2.168232944	0.030140967	NA	down_regulated
stress	under	CvsDr	TRINITY_DN70676_c0_g1	42.38715481	-7.634847234	2.827784286	-2.699939763	0.006935203	0.086631267	down_regulated
stress	under	CvsDr	TRINITY_DN1893879_c0_g1	13.2205802	-7.564692105	3.540449781	-2.136647198	0.032626698	NA	down_regulated
stress	under	CvsDr	TRINITY_DN79172_c0_g1	12.64399086	-7.500132041	3.075763116	-2.438462182	0.014749902	NA	down_regulated
stress	under	CvsDr	TRINITY_DN550370_c0_g1	12.61964474	-7.49760398	3.546342795	-2.114179145	0.034499969	NA	down_regulated
stress	under	CvsDr	TRINITY_DN3673_c2_g1	12.31645531	-7.462306043	3.083625527	-2.419978035	0.015521445	NA	down_regulated
stress	under	CvsDr	TRINITY_DN14855_c0_g1	12.26231967	-7.456039919	3.094846394	-2.409179316	0.01598844	NA	down_regulated
stress	under	CvsDr	TRINITY_DN36619_c1_g1	12.15132669	-7.442591931	3.071620345	-2.423018177	0.015392156	NA	down_regulated
stress	under	CvsDr	TRINITY_DN37283_c1_g1	11.71824154	-7.390731985	3.55656	-2.07805632	0.037704172	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1256304_c0_g1	11.41505211	-7.352638903	3.094231229	-2.376240933	0.017490038	NA	down_regulated
stress	under	CvsDr	TRINITY_DN58476_c0_g1	10.87097399	-7.28262048	3.212636736	-2.266867087	0.023398346	NA	down_regulated
stress	under	CvsDr	TRINITY_DN12192_c0_g1	10.18339167	-7.187208816	3.116512974	-2.306170029	0.021101134	NA	down_regulated
stress	under	CvsDr	TRINITY_DN45274_c0_g1	9.991195226	-7.159426409	3.153186714	-2.27053678	0.023175034	NA	down_regulated
stress	under	CvsDr	TRINITY_DN20197_c0_g1	9.723238704	-7.121614923	3.178916765	-2.240264672	0.025073745	NA	down_regulated
stress	under	CvsDr	TRINITY_DN566137_c0_g1	9.723238704	-7.121614923	3.178916765	-2.240264672	0.025073745	NA	down_regulated
stress	under	CvsDr	TRINITY_DN8395_c6_g1	9.614967418	-7.105457238	3.589330097	-1.979605399	0.047747886	NA	down_regulated
stress	under	CvsDr	TRINITY_DN5298_c1_g1	63.05670777	-7.087667336	2.653354528	-2.671210071	0.007557833	0.092151384	down_regulated
stress	under	CvsDr	TRINITY_DN62418_c0_g1	9.395703151	-7.072251675	3.204973377	-2.206649118	0.027338583	NA	down_regulated
stress	under	CvsDr	TRINITY_DN226036_c0_g1	9.227852833	-7.045030361	3.138884209	-2.244437798	0.024804247	NA	down_regulated
stress	under	CvsDr	TRINITY_DN2270_c8_g1	9.203506706	-7.042168434	3.153179062	-2.233355067	0.025525539	NA	down_regulated
stress	under	CvsDr	TRINITY_DN26615_c1_g2	9.014031954	-7.012393425	3.601931069	-1.94684276	0.051553589	NA	down_regulated
stress	under	CvsDr	TRINITY_DN175469_c0_g1	8.927385101	-6.997211323	3.150125191	-2.221248648	0.026334125	NA	down_regulated
stress	under	CvsDr	TRINITY_DN209240_c0_g1	8.220900045	-6.879533267	3.208846299	-2.143927326	0.03203871	NA	down_regulated
stress	under	CvsDr	TRINITY_DN9408_c0_g1	8.220900045	-6.879533267	3.208846299	-2.143927326	0.03203871	NA	down_regulated
stress	under	CvsDr	TRINITY_DN88874_c0_g1	8.134253192	-6.862448479	3.264951467	-2.101853136	0.035566143	NA	down_regulated
stress	under	CvsDr	TRINITY_DN26681_c1_g1	8.001635778	-6.840256221	3.181598412	-2.14994331	0.031559699	NA	down_regulated
stress	under	CvsDr	TRINITY_DN109233_c0_g1	7.998914084	-6.838640146	3.185813945	-2.14659119	0.031825842	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1164312_c0_g1	24.8815446	-6.825367974	3.265514081	-2.090135827	0.0366056	0.20426554	down_regulated
stress	under	CvsDr	TRINITY_DN1617193_c0_g1	7.86629667	-6.81609958	3.266646744	-2.086573821	0.036926676	NA	down_regulated
stress	under	CvsDr	TRINITY_DN72868_c1_g1	7.674100225	-6.780051981	3.198974332	-2.119445571	0.034052829	NA	down_regulated
stress	under	CvsDr	TRINITY_DN18374_c0_g1	7.538761117	-6.754830263	3.328760022	-2.029233173	0.042434546	NA	down_regulated
stress	under	CvsDr	TRINITY_DN16478_c0_g6	7.343842977	-6.715392216	3.205631317	-2.094873537	0.036182236	NA	down_regulated
stress	under	CvsDr	TRINITY_DN65048_c0_g1	7.073164761	-6.66239359	3.21861538	-2.069956426	0.038456425	NA	down_regulated
stress	under	CvsDr	TRINITY_DN160073_c0_g1	22.17733501	-6.657459653	3.269203834	-2.036416201	0.04170858	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1878999_c0_g1	6.496575425	-6.537971688	3.315906354	-1.971699738	0.048643894	NA	down_regulated
stress	under	CvsDr	TRINITY_DN398341_c0_g1	6.418093654	-6.522433666	3.266641736	-1.996678605	0.045860108	NA	down_regulated
stress	under	CvsDr	TRINITY_DN9244_c4_g1	6.418093654	-6.522433666	3.266641736	-1.996678605	0.045860108	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1812170_c0_g1	6.391025833	-6.516471712	3.286056689	-1.983067344	0.047359908	NA	down_regulated
stress	under	CvsDr	TRINITY_DN19871_c1_g1	6.363958011	-6.510472873	3.315911027	-1.96340397	0.049599253	NA	down_regulated
stress	under	CvsDr	TRINITY_DN6027_c0_g1	6.33689019	-6.504422725	3.369014777	-1.930660194	0.053525089	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1841662_c1_g1	6.14197205	-6.457080271	3.301960485	-1.955529238	0.050520636	NA	down_regulated
stress	under	CvsDr	TRINITY_DN238626_c0_g1	5.898361656	-6.400096728	3.264433889	-1.960553329	0.049931151	NA	down_regulated
stress	under	CvsDr	TRINITY_DN67013_c0_g2	5.70616521	-6.351524597	3.277959207	-1.937646015	0.052666424	NA	down_regulated
stress	under	CvsDr	TRINITY_DN124705_c0_g1	5.652029567	-6.338128115	3.275913897	-1.934766393	0.053018969	NA	down_regulated
stress	under	CvsDr	TRINITY_DN162757_c0_g2	5.624961745	-6.331385542	3.277964878	-1.931498896	0.053421386	NA	down_regulated
stress	under	CvsDr	TRINITY_DN37231_c0_g2	25.58450237	-6.312620118	2.687948574	-2.348489915	0.01884971	0.138031639	down_regulated
stress	under	CvsDr	TRINITY_DN275587_c0_g1	73.38548571	-6.247420996	3.1401226	-1.98954684	0.046640877	0.236241366	down_regulated
stress	under	CvsDr	TRINITY_DN137524_c0_g1	5.297426192	-6.244951877	3.302031012	-1.891245677	0.058591557	NA	down_regulated
stress	under	CvsDr	TRINITY_DN18209_c0_g1	612.9440024	-5.661179285	2.202590865	-2.570236432	0.010162913	0.099501893	down_regulated
stress	under	CvsDr	TRINITY_DN98015_c0_g1	10.67563604	-5.613769336	2.916996423	-1.924503332	0.054291523	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1380_c2_g1	22.21107507	-5.538532023	2.663949193	-2.079068189	0.037611083	NA	down_regulated
stress	under	CvsDr	TRINITY_DN59636_c0_g1	9.963707597	-5.512166414	2.906901671	-1.896234217	0.05792908	NA	down_regulated
stress	under	CvsDr	TRINITY_DN14245_c0_g2	29.05040915	-5.055037646	2.442765302	-2.069391456	0.038509368	0.210232622	down_regulated
stress	under	CvsDr	TRINITY_DN10446_c0_g2	31.20701333	-5.050193577	2.657796096	-1.90014335	0.05741431	0.263545208	down_regulated
stress	under	CvsDxD	TRINITY_DN118452_c1_g1	47.10271251	-9.266031538	3.379856205	-2.741546083	0.006115078	0.064459963	down_regulated
stress	under	CvsDxD	TRINITY_DN45557_c0_g1	23.67729934	-8.274292673	3.389607073	-2.441077238	0.014643522	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN106737_c0_g1	19.2266988	-7.974425184	3.127426403	-2.549836241	0.010777353	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN160073_c0_g1	18.38768991	-7.909838946	3.403880765	-2.323770864	0.020137778	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN86373_c0_g1	14.58554297	-7.572331319	3.02834636	-2.500483901	0.012402377	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1323639_c0_g1	13.60185281	-7.475366957	3.431993409	-2.178141408	0.029395511	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN114717_c0_g1	13.09808048	-7.42098579	3.436532129	-2.159440247	0.030816028	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN861150_c0_g1	11.83864967	-7.27532235	3.449966333	-2.108809666	0.034961014	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN89013_c0_g1	10.07544653	-7.042990359	3.475608754	-2.026404828	0.042723317	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN16478_c0_g6	7.126988162	-6.538794521	3.12901009	-2.089732642	0.036641823	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1878999_c0_g1	6.538030474	-6.412798526	3.233894416	-1.98299564	0.047367917	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN65048_c0_g1	6.293484156	-6.36343808	3.156428179	-2.016024987	0.043797363	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1841662_c1_g1	6.11944351	-6.317502024	3.222219061	-1.960606	0.049925002	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN978384_c0_g1	13.8754722	-6.067914179	2.877770647	-2.108546831	0.034983716	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN14889_c0_g1	31.80469119	-5.738435337	2.60790374	-2.200401514	0.027778421	0.139462498	down_regulated
stress	under	CvsDxD	TRINITY_DN30677_c0_g2	38.41926354	-4.370840225	2.260221981	-1.933810158	0.053136473	0.229151042	down_regulated
stress	under	CvsDi	TRINITY_DN280_c15_g1	119.3637195	4.476687798	2.163970861	2.06873756	0.038570721	0.239040198	up_regulated
stress	under	CvsDi	TRINITY_DN786_c0_g2	64.49368631	4.691135936	2.262510631	2.073420505	0.038133162	0.238177929	up_regulated
stress	under	CvsDi	TRINITY_DN283508_c0_g1	19.76433358	5.136069339	2.579678797	1.990972421	0.046483919	0.277477629	up_regulated
stress	under	CvsDi	TRINITY_DN1380_c1_g3	52.2091289	5.257875092	2.576760388	2.040498261	0.041300724	0.252739651	up_regulated
stress	under	CvsDi	TRINITY_DN5756_c0_g5	27.29767444	5.341115484	2.7329075	1.954371117	0.050657344	0.292598074	up_regulated
stress	under	CvsDi	TRINITY_DN38134_c1_g1	28.90769919	5.562623418	2.396212762	2.321423	0.020264025	0.164412176	up_regulated
stress	under	CvsDi	TRINITY_DN5940_c0_g2	71.14012467	5.71652546	2.461982527	2.32191959	0.020237266	0.164412176	up_regulated
stress	under	CvsDi	TRINITY_DN36958_c1_g1	46.66968682	5.819190966	2.542332532	2.288918107	0.022084111	0.173152754	up_regulated
stress	under	CvsDi	TRINITY_DN1810_c1_g3	40.81062983	5.939762787	2.521462872	2.355681241	0.018488775	0.162153655	up_regulated
stress	under	CvsDi	TRINITY_DN3223_c0_g2	6.784695748	5.948407898	3.072514076	1.93600672	0.052866878	NA	up_regulated
stress	under	CvsDi	TRINITY_DN16143_c1_g2	6.807332102	5.953138458	3.095653491	1.923063571	0.054472067	NA	up_regulated
stress	under	CvsDi	TRINITY_DN54996_c0_g1	7.162277874	6.026658497	3.034880906	1.985797362	0.047055829	NA	up_regulated
stress	under	CvsDi	TRINITY_DN53354_c1_g1	7.184914228	6.031137606	3.043361164	1.981735746	0.047508824	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1390809_c0_g1	7.219023539	6.038484633	3.153463144	1.914874015	0.055508572	NA	up_regulated
stress	under	CvsDi	TRINITY_DN2270_c0_g2	134.4062495	6.09893509	2.280388355	2.674515977	0.007483721	0.112025547	up_regulated
stress	under	CvsDi	TRINITY_DN8655_c1_g1	7.577587256	6.107964385	3.016304262	2.024982845	0.042869126	NA	up_regulated
stress	under	CvsDi	TRINITY_DN80153_c0_g1	7.811805808	6.152321543	3.18693402	1.930482873	0.053547035	NA	up_regulated
stress	under	CvsDi	TRINITY_DN29671_c0_g3	8.219569739	6.225714679	3.119403664	1.995802836	0.045955388	NA	up_regulated
stress	under	CvsDi	TRINITY_DN124331_c0_g2	8.257296995	6.232252887	3.027522715	2.058532164	0.039539077	NA	up_regulated
stress	under	CvsDi	TRINITY_DN20729_c0_g1	23.99565349	6.241526733	2.607865399	2.393346963	0.016695445	0.157715219	up_regulated
stress	under	CvsDi	TRINITY_DN100449_c0_g1	8.42722443	6.261695452	3.082901623	2.03110453	0.042244392	NA	up_regulated
stress	under	CvsDi	TRINITY_DN155697_c0_g1	8.578133456	6.286955846	2.969776744	2.116979285	0.034261606	NA	up_regulated
stress	under	CvsDi	TRINITY_DN26319_c0_g1	8.66113342	6.300655663	3.009022612	2.093921008	0.036267017	NA	up_regulated
stress	under	CvsDi	TRINITY_DN513972_c0_g1	9.020006699	6.359759887	3.097576457	2.053140568	0.040058946	NA	up_regulated
stress	under	CvsDi	TRINITY_DN143099_c0_g1	9.220115939	6.391411506	3.092767053	2.06656738	0.038774937	NA	up_regulated
stress	under	CvsDi	TRINITY_DN39369_c0_g1	9.605243516	6.450380361	3.416236512	1.88815392	0.05900529	NA	up_regulated
stress	under	CvsDi	TRINITY_DN71792_c0_g1	9.605243516	6.450380361	3.416236512	1.88815392	0.05900529	NA	up_regulated
stress	under	CvsDi	TRINITY_DN793182_c0_g1	9.605243516	6.450380361	3.416236512	1.88815392	0.05900529	NA	up_regulated
stress	under	CvsDi	TRINITY_DN96374_c0_g1	9.605243516	6.450380361	3.416236512	1.88815392	0.05900529	NA	up_regulated
stress	under	CvsDi	TRINITY_DN56194_c1_g2	9.612788967	6.451570642	3.138529911	2.055602726	0.039820825	NA	up_regulated
stress	under	CvsDi	TRINITY_DN4778_c2_g1	9.759770486	6.472919029	3.414680488	1.895614846	0.058010993	NA	up_regulated
stress	under	CvsDi	TRINITY_DN112154_c0_g1	9.907061567	6.494564667	2.978462538	2.180509099	0.029219745	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1152274_c0_g1	10.005462	6.509265734	3.406290242	1.91095452	0.056010426	NA	up_regulated
stress	under	CvsDi	TRINITY_DN57690_c0_g1	10.40568048	6.565841502	3.397166571	1.932740525	0.05326817	NA	up_regulated
stress	under	CvsDi	TRINITY_DN71908_c0_g1	10.41322593	6.566942628	3.122847409	2.102870159	0.035477121	NA	up_regulated
stress	under	CvsDi	TRINITY_DN196149_c3_g3	10.59038925	6.590770947	3.395599114	1.940974398	0.052261383	NA	up_regulated
stress	under	CvsDi	TRINITY_DN17284_c1_g1	10.60578972	6.593318529	3.392884269	1.943278345	0.051982536	NA	up_regulated
stress	under	CvsDi	TRINITY_DN16406_c1_g2	10.62088062	6.595428035	3.064709378	2.152056597	0.031392897	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1312504_c0_g1	32.55538766	6.666571483	3.08457242	2.161262754	0.030675048	0.207269597	up_regulated
stress	under	CvsDi	TRINITY_DN22872_c0_g2	11.24384469	6.677634689	2.985502547	2.236686984	0.025306805	NA	up_regulated
stress	under	CvsDi	TRINITY_DN73258_c1_g1	11.40622668	6.698274266	3.377390238	1.983269268	0.04733736	NA	up_regulated
stress	under	CvsDi	TRINITY_DN60930_c0_g1	11.42100802	6.699718854	3.379520108	1.982446809	0.047429255	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1599398_c0_g1	11.60633591	6.723362039	3.373883203	1.992766683	0.046287002	NA	up_regulated
stress	under	CvsDi	TRINITY_DN542189_c0_g1	13.28990024	6.91838403	3.351398715	2.06432735	0.038986689	NA	up_regulated
stress	under	CvsDi	TRINITY_DN147385_c0_g2	35.93621377	6.939223421	2.73159412	2.540356698	0.011073946	0.132698631	up_regulated
stress	under	CvsDi	TRINITY_DN105060_c0_g1	13.60742831	6.952815973	3.345086913	2.078515792	0.037661878	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1426_c2_g1	13.80753755	6.973875158	3.342724369	2.086284835	0.03695283	NA	up_regulated
stress	under	CvsDi	TRINITY_DN35219_c0_g1	13.80753755	6.973875158	3.342724369	2.086284835	0.03695283	NA	up_regulated
stress	under	CvsDi	TRINITY_DN40491_c0_g1	40.60017378	6.999958751	2.79274671	2.506478202	0.012194058	0.136377219	up_regulated
stress	under	CvsDi	TRINITY_DN21593_c0_g1	14.40786527	7.035268874	3.336089379	2.108837046	0.03495865	NA	up_regulated
stress	under	CvsDi	TRINITY_DN127987_c0_g1	14.43804708	7.038339302	2.966196438	2.372850028	0.017651429	NA	up_regulated
stress	under	CvsDi	TRINITY_DN679434_c0_g1	15.00819299	7.094155958	3.330068571	2.130333297	0.033144106	NA	up_regulated
stress	under	CvsDi	TRINITY_DN38375_c0_g1	15.40841147	7.13211971	3.326359564	2.144121696	0.032023137	NA	up_regulated
stress	under	CvsDi	TRINITY_DN80787_c1_g1	16.60906691	7.240361288	3.316496407	2.183135574	0.029025827	0.198280917	up_regulated
stress	under	CvsDi	TRINITY_DN12968_c0_g1	17.43214023	7.310183626	2.968828202	2.462312781	0.013804422	0.146121128	up_regulated
stress	under	CvsDi	TRINITY_DN12694_c0_g1	18.06264975	7.361433291	2.901188877	2.537385052	0.011168404	0.132698631	up_regulated
stress	under	CvsDi	TRINITY_DN19428_c0_g1	18.47795913	7.394221042	2.881166025	2.566398804	0.01027606	0.132419799	up_regulated
stress	under	CvsDi	TRINITY_DN257495_c0_g4	19.09668614	7.441411827	3.048516519	2.440994425	0.014646881	0.148452241	up_regulated
stress	under	CvsDi	TRINITY_DN1794_c11_g1	19.21048703	7.450263985	3.300160604	2.257545883	0.023973986	0.180165443	up_regulated
stress	under	CvsDi	TRINITY_DN66082_c0_g1	20.21103323	7.523506329	3.295258421	2.283130901	0.022422659	0.173152754	up_regulated
stress	under	CvsDi	TRINITY_DN113017_c0_g1	20.62634261	7.552893903	2.987136726	2.52847278	0.011455997	0.132698631	up_regulated
stress	under	CvsDi	TRINITY_DN1083884_c0_g1	20.81136095	7.565730836	3.292607212	2.297793314	0.021573556	0.170659102	up_regulated
stress	under	CvsDi	TRINITY_DN55030_c0_g1	21.41168867	7.606754553	3.290149863	2.311978138	0.020778888	0.167089737	up_regulated
stress	under	CvsDi	TRINITY_DN14448_c0_g1	21.81190715	7.633469615	3.28861091	2.321183571	0.020276938	0.164412176	up_regulated
stress	under	CvsDi	TRINITY_DN28119_c0_g1	22.01139727	7.646368072	3.290248037	2.323948829	0.020128237	0.164412176	up_regulated
stress	under	CvsDi	TRINITY_DN12365_c0_g1	35.01911698	8.31644732	3.263269183	2.54850178	0.010818673	0.132698631	up_regulated
stress	under	CvsDr	TRINITY_DN341950_c0_g1	100.6415073	4.724952314	2.298128514	2.056000039	0.039782512	0.214012494	up_regulated
stress	under	CvsDr	TRINITY_DN1682_c0_g1	72.51484865	5.228085426	2.362737699	2.212723582	0.026916709	0.163947225	up_regulated
stress	under	CvsDr	TRINITY_DN803_c12_g1	15.63211307	5.624717843	2.857958278	1.968089557	0.049057734	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1810_c1_g3	33.65752034	5.668039772	2.777033379	2.041041283	0.041246724	0.220281416	up_regulated
stress	under	CvsDr	TRINITY_DN8655_c1_g1	7.662617341	6.054054809	3.175387011	1.90655652	0.05657804	NA	up_regulated
stress	under	CvsDr	TRINITY_DN35414_c0_g2	8.362318742	6.180805815	3.163399361	1.953849359	0.050719035	NA	up_regulated
stress	under	CvsDr	TRINITY_DN71644_c0_g1	8.505259875	6.201894196	3.154607682	1.965979551	0.049300973	NA	up_regulated
stress	under	CvsDr	TRINITY_DN85121_c0_g1	8.588460291	6.215093382	3.174037913	1.95810307	0.05021792	NA	up_regulated
stress	under	CvsDr	TRINITY_DN56911_c0_g1	8.713260915	6.234906451	3.2645263	1.909896223	0.056146576	NA	up_regulated
stress	under	CvsDr	TRINITY_DN13992_c0_g1	8.842289255	6.262921891	3.313628778	1.890049342	0.058751361	NA	up_regulated
stress	under	CvsDr	TRINITY_DN26127_c0_g1	9.210280465	6.320609857	3.157196247	2.001969267	0.045288037	NA	up_regulated
stress	under	CvsDr	TRINITY_DN51446_c0_g1	9.697753111	6.390005179	3.17902349	2.010052835	0.044425597	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1026_c6_g1	56.40095213	6.422851622	3.182085892	2.018440683	0.043545386	0.22760957	up_regulated
stress	under	CvsDr	TRINITY_DN31659_c0_g1	10.0998424	6.453586383	3.142123861	2.053893057	0.039986043	NA	up_regulated
stress	under	CvsDr	TRINITY_DN77467_c0_g2	10.41982274	6.499305644	3.234987791	2.009066514	0.04453008	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1177410_c0_g1	10.60436408	6.522325455	3.094971076	2.10739464	0.035083386	NA	up_regulated
stress	under	CvsDr	TRINITY_DN77002_c0_g1	12.30560671	6.738501797	3.565739039	1.889791071	0.058785908	NA	up_regulated
stress	under	CvsDr	TRINITY_DN58403_c0_g1	13.13229168	6.828125665	3.587890414	1.903103183	0.057027083	NA	up_regulated
stress	under	CvsDr	TRINITY_DN109376_c0_g1	14.34824462	6.956038248	3.577775649	1.94423545	0.051867065	NA	up_regulated
stress	under	CvsDr	TRINITY_DN64184_c0_g1	14.38125122	6.963085369	3.547138599	1.963014744	0.049644461	NA	up_regulated
stress	under	CvsDr	TRINITY_DN760592_c0_g1	14.5914352	6.980314549	3.57600017	1.951989434	0.050939459	NA	up_regulated
stress	under	CvsDr	TRINITY_DN5724_c1_g1	16.07185548	7.123243467	3.076789696	2.315154486	0.020604479	NA	up_regulated
stress	under	CvsDr	TRINITY_DN16478_c0_g3	16.13691538	7.129296694	3.11203274	2.290881006	0.021970297	NA	up_regulated
stress	under	CvsDr	TRINITY_DN143055_c2_g1	16.16037508	7.131161989	3.535801484	2.01684456	0.043711737	NA	up_regulated
stress	under	CvsDr	TRINITY_DN19332_c0_g1	16.4345275	7.153976306	3.018661444	2.369916746	0.017792091	NA	up_regulated
stress	under	CvsDr	TRINITY_DN8736_c0_g1	46.31810569	7.168674358	2.950144779	2.429939849	0.015101329	0.11593416	up_regulated
stress	under	CvsDr	TRINITY_DN2815_c3_g4	16.64675626	7.174255664	3.190480746	2.248644086	0.024535149	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1632280_c0_g1	17.02334107	7.202949874	3.561698352	2.022335741	0.043141679	NA	up_regulated
stress	under	CvsDr	TRINITY_DN3612_c2_g1	17.99610342	7.283200096	3.557359767	2.047361125	0.04062264	NA	up_regulated
stress	under	CvsDr	TRINITY_DN3423_c0_g1	19.51703243	7.40345415	3.235540035	2.288166448	0.02212783	NA	up_regulated
stress	under	CvsDr	TRINITY_DN109422_c1_g1	20.45992442	7.471161684	3.518644235	2.123306929	0.03372814	NA	up_regulated
stress	under	CvsDr	TRINITY_DN3382_c7_g1	22.3873086	7.60092864	3.513866882	2.163123674	0.030531669	NA	up_regulated
stress	under	CvsDr	TRINITY_DN39465_c0_g1	23.34629633	7.659036658	3.542012791	2.162340203	0.030591963	NA	up_regulated
stress	under	CvsDr	TRINITY_DN2386_c7_g1	23.58948691	7.673998699	3.541554602	2.166844666	0.0302467	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1648240_c0_g1	25.04863044	7.760650377	3.539107399	2.192827033	0.028319843	0.166973797	up_regulated
stress	under	CvsDr	TRINITY_DN57265_c0_g1	25.20425472	7.771779164	3.508868517	2.214896091	0.026767197	0.163947225	up_regulated
stress	under	CvsDr	TRINITY_DN14644_c1_g1	27.57751134	7.901366963	2.994292997	2.638808884	0.008319787	0.095807546	up_regulated
stress	under	CvsDr	TRINITY_DN9947_c0_g1	29.35554373	7.991589667	3.504357895	2.280471888	0.022579716	0.149921179	up_regulated
stress	under	CvsDr	TRINITY_DN124934_c0_g1	32.32075017	8.130330122	3.502495945	2.321296084	0.020270869	0.141046167	up_regulated
stress	under	CvsDr	TRINITY_DN5756_c0_g3	33.31711038	8.172443418	3.531771757	2.31397836	0.020668909	0.141046167	up_regulated
stress	under	CvsDr	TRINITY_DN1065_c0_g1	98.3837409	8.303461093	2.996349817	2.77119215	0.005585146	0.076888617	up_regulated
stress	under	CvsDr	TRINITY_DN29226_c0_g1	38.79305722	8.393233828	2.95000999	2.845154374	0.00443899	0.072830271	up_regulated
stress	under	CvsDr	TRINITY_DN17149_c0_g3	57.52500488	8.961669993	3.503547949	2.557884214	0.010531115	0.099505535	up_regulated
stress	under	CvsDr	TRINITY_DN3929_c0_g1	62.98636197	9.091596242	3.534112955	2.572525654	0.010095947	0.099501893	up_regulated
stress	under	CvsDr	TRINITY_DN187065_c0_g1	64.19671937	9.11992723	3.505561164	2.601559865	0.009280087	0.099501893	up_regulated
stress	under	CvsDr	TRINITY_DN58887_c0_g1	64.49324001	9.12657348	3.505655548	2.603385688	0.009230804	0.099501893	up_regulated
stress	under	CvsDr	TRINITY_DN4640_c0_g1	101.7065808	22.51893133	3.51819125	6.400712676	1.55E-10	8.34E-09	up_regulated
stress	under	CvsDxD	TRINITY_DN1290_c0_g1	1157.864747	3.958230881	2.088955467	1.894837369	0.058113951	0.237142412	up_regulated
stress	under	CvsDxD	TRINITY_DN3496_c0_g1	159.2718194	4.109686877	2.085310688	1.970779175	0.04874914	0.21511975	up_regulated
stress	under	CvsDxD	TRINITY_DN30848_c0_g1	63.14519751	4.187689488	2.197192412	1.905927521	0.05665961	0.234081154	up_regulated
stress	under	CvsDxD	TRINITY_DN341950_c0_g1	63.65104914	4.235659148	2.217667597	1.909961238	0.056138204	0.234081154	up_regulated
stress	under	CvsDxD	TRINITY_DN2815_c0_g1	130.3639674	4.260243378	2.168875959	1.964263268	0.049499569	0.21716863	up_regulated
stress	under	CvsDxD	TRINITY_DN1266006_c0_g1	31.94109133	4.495101808	2.342686188	1.918781026	0.055012052	0.233201153	up_regulated
stress	under	CvsDxD	TRINITY_DN47749_c0_g1	20.56059303	4.545909386	2.36701329	1.920525501	0.054791555	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN63119_c0_g1	27.7808005	4.598998895	2.363970273	1.945455468	0.051720184	0.224317828	up_regulated
stress	under	CvsDxD	TRINITY_DN26857_c0_g1	68.27124546	4.615536722	2.316230217	1.992693424	0.046295028	0.205484948	up_regulated
stress	under	CvsDxD	TRINITY_DN19689_c0_g1	141.1623797	4.993578258	2.364827361	2.111603722	0.034720453	0.16574103	up_regulated
stress	under	CvsDxD	TRINITY_DN3439_c3_g1	142.7776884	4.994371753	2.259740383	2.210152897	0.027094553	0.138018561	up_regulated
stress	under	CvsDxD	TRINITY_DN147385_c0_g1	44.57842779	5.036909543	2.615036796	1.926133334	0.054087727	0.231935509	up_regulated
stress	under	CvsDxD	TRINITY_DN786_c0_g2	84.03390768	5.12329754	2.254338747	2.272638727	0.02304796	0.125935441	up_regulated
stress	under	CvsDxD	TRINITY_DN29820_c0_g1	19.53134305	5.180772569	2.631838026	1.968499777	0.049010562	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1682_c0_g1	67.88344926	5.2334347	2.38348609	2.195705996	0.028112994	0.139462498	up_regulated
stress	under	CvsDxD	TRINITY_DN3673_c0_g1	144.0183431	5.260305897	2.535555151	2.07461703	0.038022042	0.174901395	up_regulated
stress	under	CvsDxD	TRINITY_DN1022055_c0_g1	23.47411397	5.455223808	2.716385802	2.00826547	0.044615089	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN4585_c0_g2	46.3909854	5.46077337	2.597086667	2.1026535	0.03549607	0.16630566	up_regulated
stress	under	CvsDxD	TRINITY_DN5756_c0_g5	31.96400004	5.593977912	2.660082574	2.102933934	0.035471545	0.16630566	up_regulated
stress	under	CvsDxD	TRINITY_DN44930_c0_g1	15.32621764	5.599394049	2.855696912	1.96078023	0.049904665	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN296357_c0_g1	15.05317007	5.71689114	2.951418302	1.936997929	0.052745596	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN48680_c0_g1	16.88933883	5.736238601	2.983380146	1.922731372	0.054513795	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1810_c1_g3	35.83924184	5.762529549	2.654943854	2.170490174	0.029969732	0.145814274	up_regulated
stress	under	CvsDxD	TRINITY_DN5467_c0_g1	80.04596188	5.782931635	2.241101321	2.580397227	0.009868673	0.086095661	up_regulated
stress	under	CvsDxD	TRINITY_DN1795_c2_g4	141.342341	5.92404902	2.664024452	2.223721714	0.026167173	0.136033456	up_regulated
stress	under	CvsDxD	TRINITY_DN63156_c0_g2	6.684805398	5.965544185	3.141272028	1.899085508	0.057553234	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN13166_c0_g1	34.02934762	6.003531938	2.769864616	2.167445984	0.030200864	0.146002904	up_regulated
stress	under	CvsDxD	TRINITY_DN61288_c1_g2	6.922615325	6.018001916	3.147222881	1.912162609	0.05585534	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN38375_c2_g1	35.39263529	6.054946437	2.612819985	2.317399006	0.020482006	0.120021733	up_regulated
stress	under	CvsDxD	TRINITY_DN135256_c0_g1	7.174171753	6.069839077	3.216304635	1.887209007	0.059132219	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN114897_c0_g1	57.08902418	6.178920363	2.623151861	2.355532844	0.018496162	0.114134852	up_regulated
stress	under	CvsDxD	TRINITY_DN4043_c3_g2	7.823682576	6.194140454	3.089887571	2.004649138	0.04500057	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN28942_c0_g1	254.4761923	6.197505248	2.349133693	2.638208829	0.008334524	0.076215707	up_regulated
stress	under	CvsDxD	TRINITY_DN1026_c0_g1	40.15038429	6.243070983	2.698202601	2.313788809	0.02067931	0.120021733	up_regulated
stress	under	CvsDxD	TRINITY_DN358003_c0_g1	8.134691486	6.250992177	3.189809993	1.959675401	0.050033742	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN527436_c0_g1	8.485736512	6.311920802	3.237002247	1.949927841	0.05118472	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN186521_c0_g1	8.685917096	6.342996619	3.159328497	2.00770405	0.04467475	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN54996_c0_g1	9.095211218	6.411904776	3.17097348	2.022061936	0.043169954	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN77693_c0_g1	9.428043267	6.461316624	3.230083935	2.000355642	0.045461875	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN36834_c0_g1	9.513785364	6.47485108	3.04978414	2.123052249	0.033749474	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN11818_c3_g1	25.56510451	6.489434035	2.715475662	2.389796427	0.016857714	0.105743842	up_regulated
stress	under	CvsDxD	TRINITY_DN32851_c0_g1	9.745925428	6.509289875	3.103742343	2.097239125	0.035972413	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN11579_c0_g1	9.751595291	6.511588147	3.024140142	2.153203172	0.031302714	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN40576_c0_g1	10.2890744	6.58773288	3.055054962	2.156338581	0.031057237	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN133849_c0_g1	10.5875402	6.628924037	3.500436684	1.89374202	0.058259261	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1340517_c0_g1	10.5875402	6.628924037	3.500436684	1.89374202	0.058259261	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN31154_c0_g1	10.64699268	6.636923752	3.090576276	2.147471267	0.031755782	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN133093_c0_g1	10.71898827	6.647772926	2.998203236	2.217252268	0.026605858	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN77467_c0_g2	10.80593376	6.658382245	3.065653473	2.171929183	0.029861005	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN8736_c0_g1	31.6962926	6.671076664	2.709236501	2.462345632	0.013803158	0.095241788	up_regulated
stress	under	CvsDxD	TRINITY_DN1823386_c0_g1	11.00491096	6.684581272	3.208625907	2.08331587	0.037222441	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN180460_c0_g1	11.01625068	6.688186774	3.143749967	2.12745506	0.033382292	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN63413_c0_g1	11.08257642	6.696792119	3.091252523	2.166368509	0.030283038	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1399958_c0_g1	11.1420289	6.70437581	3.48231651	1.925263195	0.05419644	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN283420_c0_g1	11.14890215	6.705323561	3.061763886	2.190019809	0.0285228	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN192722_c0_g1	11.20835463	6.71311317	3.141643153	2.136815941	0.032612965	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN38474_c1_g1	11.34100609	6.729961431	3.059620635	2.199606498	0.027834826	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN159957_c0_g1	11.36729571	6.733412236	3.194165041	2.108035167	0.035027948	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN13616_c1_g2	11.51489706	6.750061568	3.068363702	2.199889656	0.027814725	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN22214_c0_g1	11.52623679	6.753258597	3.477260506	1.942120409	0.052122525	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN39369_c0_g1	11.52623679	6.753258597	3.477260506	1.942120409	0.052122525	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN31659_c0_g1	11.92539457	6.800883435	3.007364202	2.261409985	0.023733881	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1878223_c0_g1	12.16440789	6.829329015	3.479748963	1.962592442	0.04969355	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN55719_c0_g1	12.32334897	6.847920173	3.14186391	2.179572499	0.029289165	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN19850_c6_g1	12.61494151	6.881822449	3.474944512	1.980412184	0.047657232	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN72369_c1_g1	12.67886047	6.890690688	3.464236323	1.989093712	0.04669086	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN186474_c0_g1	12.77388259	6.899751823	3.138245469	2.1986017	0.027906256	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN16143_c1_g2	12.96478315	6.922210933	2.962570643	2.336555569	0.019462305	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN4043_c1_g1	13.10310448	6.938214292	3.032805253	2.287721668	0.022153735	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN6998_c0_g1	126.7064261	6.938844164	3.08007835	2.252814174	0.024270868	0.129729499	up_regulated
stress	under	CvsDxD	TRINITY_DN35475_c0_g1	13.25517231	6.954789296	3.45873192	2.01079166	0.044347467	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN56764_c0_g3	13.51600876	6.981405473	3.466481363	2.013974616	0.044012199	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1050344_c0_g1	13.74127557	7.00526319	3.46457583	2.021968499	0.043179606	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN31568_c0_g1	134.063584	7.012718635	2.611953851	2.684855489	0.007256117	0.068954502	up_regulated
stress	under	CvsDxD	TRINITY_DN1063_c1_g2	36.80541862	7.020390484	2.763557279	2.540345568	0.011074299	0.088322486	up_regulated
stress	under	CvsDxD	TRINITY_DN291754_c0_g1	13.96654239	7.028732839	3.462745912	2.029814782	0.04237537	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN706070_c0_g1	14.21569204	7.055670265	3.450758084	2.044672531	0.040887154	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN7640_c3_g1	14.60677319	7.094911504	3.033500447	2.338852961	0.019343045	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN168873_c0_g1	15.9939437	7.224366797	3.449128336	2.094548562	0.036211142	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN16580_c0_g1	16.44327394	7.264808107	2.928668478	2.480583979	0.013116736	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1611870_c0_g1	44.40418145	7.296814046	3.21798671	2.267509068	0.023359146	0.126639939	up_regulated
stress	under	CvsDxD	TRINITY_DN16478_c0_g1	17.00463589	7.314149773	3.07469537	2.378820954	0.01736811	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN49415_c6_g1	18.05777097	7.400660991	3.429212014	2.158122904	0.030918277	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN24033_c0_g1	18.11275698	7.404341514	2.915494539	2.539651993	0.011096282	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN55762_c1_g1	18.38493316	7.425595331	2.937683605	2.527704249	0.011481102	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1803132_c0_g1	19.04131723	7.476023318	2.991115692	2.499409614	0.012440043	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN105850_c1_g1	19.72399092	7.526802163	3.078502315	2.444955824	0.014486989	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1842629_c0_g1	20.04513617	7.551371662	3.097378287	2.437988183	0.014769257	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN6578_c0_g2	20.15837127	7.559341579	2.941554077	2.569846204	0.010174367	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN280_c14_g1	20.68777374	7.596880273	3.044170914	2.495549852	0.012576208	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN4778_c2_g1	23.05247358	7.752841165	3.414915732	2.270287694	0.023190133	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN28119_c0_g1	23.24457753	7.764810149	3.414546388	2.27403856	0.022963668	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN2386_c0_g2	26.03146167	7.927247751	3.07101508	2.581311894	0.009842561	0.086095661	up_regulated
stress	under	CvsDxD	TRINITY_DN9743_c0_g1	26.5814839	7.957499755	3.41940223	2.327161071	0.019956694	0.11833696	up_regulated
stress	under	CvsDxD	TRINITY_DN1044_c14_g1	27.25247079	7.994351372	3.02153005	2.645795752	0.008149901	0.075436278	up_regulated
stress	under	CvsDxD	TRINITY_DN280_c0_g2	276.0125942	8.058061996	2.524355256	3.192126773	0.001412293	0.024928619	up_regulated
stress	under	CvsDxD	TRINITY_DN3443_c9_g1	29.58194806	8.111858587	2.916142794	2.78170829	0.005407362	0.063141349	up_regulated
stress	under	CvsDxD	TRINITY_DN39465_c0_g1	30.4587854	8.154766209	2.977441508	2.73885018	0.006165446	0.064459963	up_regulated
stress	under	CvsDxD	TRINITY_DN16775_c0_g1	32.1731181	8.232940179	2.997583865	2.746525385	0.006023022	0.064459963	up_regulated
stress	under	CvsDxD	TRINITY_DN39544_c0_g1	34.1742298	8.32001344	3.099496055	2.684311672	0.007267932	0.068954502	up_regulated
stress	under	CvsDxD	TRINITY_DN101231_c1_g2	34.91635597	8.351068314	3.413821044	2.446252515	0.014434986	0.097822808	up_regulated
stress	under	CvsDxD	TRINITY_DN1951_c0_g3	220.5733101	8.556735617	2.695390163	3.174581452	0.001500528	0.025155425	up_regulated
stress	under	CvsDxD	TRINITY_DN4123_c1_g1	40.28237628	8.557962075	3.039308036	2.815760026	0.0048662	0.059571706	up_regulated
stress	under	CvsDxD	TRINITY_DN47749_c0_g2	41.22382672	8.590686381	3.412996986	2.517050679	0.011834181	0.091654521	up_regulated
stress	under	CvsDxD	TRINITY_DN280_c3_g1	41.87866034	8.613925834	3.403294591	2.531055012	0.011372001	0.088982978	up_regulated
stress	under	CvsDxD	TRINITY_DN1251980_c0_g1	47.78938009	8.80437653	2.868449666	3.069385053	0.002144999	0.031922637	up_regulated
stress	under	CvsDxD	TRINITY_DN1762_c0_g2	134.786508	8.899764469	2.741797615	3.245959665	0.001170554	0.021669528	up_regulated
stress	under	CvsDxD	TRINITY_DN803_c12_g1	143.2722445	8.98802927	2.744010609	3.275508207	0.00105472	0.020054689	up_regulated
stress	under	CvsDxD	TRINITY_DN763605_c0_g1	66.49545849	9.280934382	2.921540235	3.176726533	0.001489474	0.025155425	up_regulated
stress	under	CvsDxD	TRINITY_DN523_c2_g1	101.2868926	9.887982608	2.899444917	3.410301934	0.00064891	0.013681185	up_regulated
stress	under	CvsDxD	TRINITY_DN51446_c0_g1	137.0307697	10.32400173	2.953871875	3.495074319	0.00047393	0.011990423	up_regulated
stress	under	CvsDxD	TRINITY_DN280_c8_g1	155.3263506	10.50478461	3.033762752	3.462625615	0.000534932	0.012303439	up_regulated
stress	under	CvsDxD	TRINITY_DN3612_c2_g1	473.5233834	12.11278501	3.055262403	3.964564548	7.35E-05	0.002325388	up_regulated
stress	under	CvsDxD	TRINITY_DN3929_c0_g1	2083.827642	14.25051563	3.053407072	4.667086731	3.06E-06	0.000122039	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


HSP70_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

HSP70_reg

HSP70_reg_final <- HSP70_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

HSP70_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

HSP70_reg_final_plot <- HSP70_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                               panel.background = element_rect(fill = "white"), 
                                                                                               panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                               axis.title.x = element_text(color="black", vjust=1),
                                                                                               axis.title.y = element_text(color="black" , vjust=1)) 






HSP70_reg_final_plot


jpeg("HSP70_regulation_under.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(HSP70_reg_final_plot)
dev.off()''





################ heat shock protein 90 inter


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	inter	CvsDr	TRINITY_DN1955_c0_g1	149.8671424	-10.54637492	3.327616103	-3.169348444	0.001527811	0.025433562	down_regulated
stress	inter	CvsDr	TRINITY_DN1955_c0_g2	55.48093547	-9.112762281	3.575371298	-2.548759701	0.010810676	0.09027918	down_regulated
stress	inter	CvsDr	TRINITY_DN879523_c0_g1	53.93365839	-9.071957138	3.575123964	-2.537522399	0.011164022	0.09027918	down_regulated
stress	inter	CvsDr	TRINITY_DN3020_c0_g1	43.32375838	-8.755938353	3.574594257	-2.449491529	0.014305807	0.101213588	down_regulated
stress	inter	CvsDr	TRINITY_DN4458_c12_g1	31.16658128	-8.280793817	3.579450063	-2.313426273	0.020699214	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN28253_c0_g1	24.97747294	-7.96143156	3.587651194	-2.219120848	0.026478504	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN9850_c0_g1	19.23044377	-7.584209055	3.603999633	-2.104386744	0.035344725	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN898327_c0_g1	17.4621271	-7.445051409	3.612219878	-2.061073706	0.039296009	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN47704_c1_g4	17.02004793	-7.408058667	3.614628686	-2.04946602	0.040416567	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN9329_c0_g2	15.85283564	-7.305504955	3.268971943	-2.234801975	0.025430352	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN15767_c0_g1	15.49581798	-7.272738438	3.15609776	-2.304345109	0.021203279	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN13085_c1_g1	15.18741217	-7.243623758	3.627183233	-1.997038278	0.045821025	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN81907_c0_g1	13.26237501	-7.048176871	3.643569373	-1.934415445	0.053062069	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN13209_c0_g1	13.04133543	-7.023930336	3.645911868	-1.92652225	0.054039196	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN76727_c0_g1	9.585367051	-6.579676297	3.235540287	-2.033563397	0.041995635	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN61989_c0_g1	9.15481145	-6.5133427	3.392347833	-1.920010276	0.054856601	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN1322_c0_g2	9.131764319	-6.509749283	3.227076345	-2.017228162	0.043671708	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN614254_c0_g1	8.429465445	-6.394370445	3.236680052	-1.975595469	0.048200612	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1955_c0_g2	65.29924645	-9.638795209	3.489582231	-2.762163082	0.005741979	0.084430584	down_regulated
stress	inter	CvsDi	TRINITY_DN879523_c0_g1	63.47815193	-9.597985974	3.489393542	-2.750617223	0.00594831	0.084430584	down_regulated
stress	inter	CvsDi	TRINITY_DN4458_c12_g1	36.68204681	-8.806716945	3.495313988	-2.519578206	0.011749554	0.10636438	down_regulated
stress	inter	CvsDi	TRINITY_DN28253_c0_g1	29.39766872	-8.487294423	3.504477504	-2.421843031	0.015442019	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1955_c0_g1	176.9950679	-8.101324038	2.921891592	-2.772629916	0.005560532	0.084430584	down_regulated
stress	inter	CvsDi	TRINITY_DN898327_c0_g1	20.55235247	-7.970788403	3.531142237	-2.2572833	0.023990378	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN15767_c0_g1	17.92675209	-7.773559797	3.087778117	-2.517525386	0.011818245	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN946_c6_g1	17.6097571	-7.747846373	3.076906696	-2.518063477	0.011800205	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN124400_c0_g1	16.24563006	-7.632465954	3.09743121	-2.464127671	0.013734718	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN13209_c0_g1	15.34922526	-7.549533487	3.567169017	-2.116393546	0.03431135	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN31404_c0_g1	14.02440669	-7.419328933	3.117046498	-2.380243265	0.017301212	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN16689_c0_g2	13.90422282	-7.407984622	3.575862509	-2.071663719	0.038296813	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1211326_c0_g1	11.44687986	-7.126173368	3.620560567	-1.96825139	0.04903912	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN8502_c0_g2	11.2523272	-7.101814216	3.130089072	-2.268885662	0.02327528	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN6617_c0_g1	10.92656714	-7.059032442	3.630926191	-1.944140991	0.051878451	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN13580_c0_g1	10.40625441	-6.988614206	3.642420386	-1.918673153	0.055025711	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN69849_c0_g1	10.14609805	-6.952072957	3.648644748	-1.905384995	0.056730044	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN23380_c2_g1	10.11767874	-6.947909601	3.340457272	-2.079927697	0.037532165	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1138519_c0_g1	9.885941694	-6.914582212	3.655220556	-1.891700407	0.058530909	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN47579_c2_g1	9.885941694	-6.914582212	3.655220556	-1.891700407	0.058530909	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN3020_c0_g1	51.41562986	-6.893425333	3.17252367	-2.172852294	0.029791436	0.22278813	down_regulated
stress	inter	CvsDi	TRINITY_DN61989_c0_g1	9.529638238	-6.863330386	3.345935945	-2.051243807	0.04024321	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN38927_c0_g1	8.399372317	-6.681257957	3.323052598	-2.010578455	0.044370002	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN47704_c1_g2	7.345599257	-6.48613671	3.284560552	-1.974735009	0.048298228	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN1955_c0_g1	149.7905294	-10.32370856	3.61426089	-2.85638167	0.004284997	0.037902473	down_regulated
stress	inter	CvsDxD	TRINITY_DN1955_c0_g2	55.46536187	-8.890347155	3.900673907	-2.279182358	0.022656227	0.103211702	down_regulated
stress	inter	CvsDxD	TRINITY_DN879523_c0_g1	53.91851911	-8.849537997	3.900197546	-2.268997376	0.023268485	0.104344614	down_regulated
stress	inter	CvsDxD	TRINITY_DN3020_c0_g1	43.31159732	-8.5334839	3.897566922	-2.189438712	0.028564969	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN4458_c12_g1	31.15783277	-8.058269336	3.897939159	-2.067315319	0.038704452	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN28253_c0_g1	24.97046172	-7.738845652	3.901983107	-1.983310906	0.047332712	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN9850_c0_g1	19.22504575	-7.361531934	3.9119032	-1.881828756	0.05985927	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN139_c0_g1	81.41918423	4.759687823	2.342190225	2.032152543	0.042138216	0.25924163	up_regulated
stress	inter	CvsDr	TRINITY_DN17920_c1_g2	7.897712618	6.594140576	3.249807615	2.029086444	0.042449486	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN4247_c0_g2	8.961563462	6.775337604	3.388656508	1.999417052	0.045563248	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN61266_c1_g1	10.15688274	6.956220375	3.680658491	1.889939094	0.058766106	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN40315_c2_g1	12.18825929	7.219385174	3.64874333	1.978594963	0.047861627	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN468_c4_g5	13.59804757	7.37775845	3.131406972	2.356052253	0.018470319	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN2932_c2_g1	44.22986648	9.079871874	3.55925592	2.551059008	0.010739614	0.09027918	up_regulated
stress	inter	CvsDr	TRINITY_DN8406_c0_g1	129.1447449	9.182359695	3.097943241	2.964018053	0.003036505	0.042966542	up_regulated
stress	inter	CvsDr	TRINITY_DN1093_c1_g1	51.13279362	9.289067466	3.559534301	2.609629991	0.00906402	0.09027918	up_regulated
stress	inter	CvsDr	TRINITY_DN689_c0_g2	60.8480244	9.539994978	3.561271368	2.678817195	0.007388272	0.087120038	up_regulated
stress	inter	CvsDi	TRINITY_DN11762_c1_g1	30.42663405	4.979442176	2.589115695	1.923221193	0.054452277	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN162830_c0_g2	11.19442691	6.952788316	3.630222792	1.915251133	0.055460484	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN1204136_c0_g1	12.11202189	7.067414544	3.607811021	1.958920382	0.050122112	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN775023_c0_g1	16.57434575	7.519810572	3.555439895	2.115015524	0.034428624	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN13045_c0_g4	17.95772651	7.634779241	3.550924418	2.150082159	0.031548717	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN35315_c0_g1	18.27427864	7.660641938	3.543123093	2.162115664	0.030609262	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN52602_c0_g1	18.27427864	7.660641938	3.543123093	2.162115664	0.030609262	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN6873_c0_g1	22.40979566	7.95459535	2.989405405	2.660928938	0.00779254	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN3481_c1_g1	23.32172273	8.011917031	3.524829823	2.272993998	0.023026542	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN35945_c0_g1	20.313444	6.650141902	3.196330254	2.080555316	0.037474627	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN263_c11_g1	41.12942637	6.731015571	3.119108566	2.157993359	0.030928348	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN57559_c0_g1	8.156722732	6.762652727	3.493600868	1.935725626	0.052901314	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN58548_c0_g2	8.412917559	6.807405398	3.518976329	1.934484566	0.053053578	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN954144_c0_g1	12.97727463	7.432571066	3.911441946	1.900212548	0.057405232	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN36036_c0_g1	13.06314786	7.441474706	3.913991461	1.901249602	0.057269327	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN373713_c0_g1	13.59524009	7.499671432	3.906875759	1.919608376	0.054907385	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN29898_c1_g1	13.90422282	7.532086399	3.904784954	1.928937569	0.053738613	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1407801_c0_g1	14.21320555	7.56378903	3.902810542	1.938036435	0.052618778	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN8072_c0_g1	14.21320555	7.56378903	3.902810542	1.938036435	0.052618778	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN36178_c0_g1	15.02262004	7.643143963	3.901346816	1.959103952	0.050100614	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN764466_c0_g1	15.14015373	7.654919571	3.897510035	1.964053845	0.049523848	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN547514_c0_g1	15.52726501	7.691139856	3.366891659	2.284344326	0.022351303	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN175631_c0_g1	16.65551352	7.792030667	3.893710718	2.001183763	0.04537259	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN11463_c0_g1	17.51207784	7.864417894	3.415204808	2.302766111	0.021292006	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1566_c3_g1	18.64243296	7.95476264	3.361908234	2.366145084	0.017974399	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN10292_c0_g1	19.2681431	8.002277419	3.885099705	2.059735406	0.039423843	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN47547_c0_g1	260.2436171	8.626564089	3.209825929	2.687548883	0.007197857	0.055832023	up_regulated
stress	inter	CvsDxD	TRINITY_DN468_c0_g1	883.2955469	8.96595536	3.601397343	2.489576824	0.012789527	0.07979553	up_regulated
stress	inter	CvsDxD	TRINITY_DN908043_c0_g1	44.41470273	9.207198264	3.870895709	2.378570479	0.017379914	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN47547_c1_g1	77.36395915	10.00786736	3.510294175	2.851005317	0.004358124	0.037902473	up_regulated
stress	inter	CvsDxD	TRINITY_DN1098572_c0_g1	77.07257239	22.7120071	3.881490938	5.851361619	4.88E-09	5.64E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN9021_c1_g1	88.82940546	22.90936071	3.885685563	5.895834941	3.73E-09	5.35E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN946_c2_g1	233.1771893	23.34064257	3.923043209	5.949626687	2.69E-09	5.20E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN18642_c0_g1	146.3072561	23.60130982	3.903511174	6.046174525	1.48E-09	3.87E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN235_c0_g2	152.83883	23.6659421	3.905247666	6.060036167	1.36E-09	3.87E-08	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


HSP90_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

HSP90_reg

HSP90_reg_final <- HSP90_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

HSP90_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

HSP90_reg_final_plot <- HSP90_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                               panel.background = element_rect(fill = "white"), 
                                                                                               panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                               axis.title.x = element_text(color="black", vjust=1),
                                                                                               axis.title.y = element_text(color="black" , vjust=1)) 






HSP90_reg_final_plot


jpeg("HSP90_regulation_inter.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(HSP90_reg_final_plot)
dev.off()






################ heat shock protein 90 under


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	under	CvsDi	TRINITY_DN6515_c1_g1	384.0485675	-25.60372382	3.294217075	-7.772324422	7.71E-15	6.47E-12	down_regulated
stress	under	CvsDi	TRINITY_DN85_c0_g4	305.6551486	-25.29021077	3.283198331	-7.70291899	1.33E-14	6.47E-12	down_regulated
stress	under	CvsDi	TRINITY_DN23199_c0_g1	189.5167501	-24.63983092	3.261898892	-7.553830373	4.23E-14	8.52E-12	down_regulated
stress	under	CvsDi	TRINITY_DN1838_c0_g1	211.6224242	-10.02371615	2.537918298	-3.949581892	7.83E-05	0.004231892	down_regulated
stress	under	CvsDi	TRINITY_DN1347375_c0_g1	58.36698557	-9.603637622	2.745218654	-3.498314282	0.000468209	0.02277837	down_regulated
stress	under	CvsDi	TRINITY_DN184957_c1_g1	16.22535974	-7.75568412	2.878576862	-2.694277239	0.007054146	NA	down_regulated
stress	under	CvsDi	TRINITY_DN9641_c0_g1	16.05860173	-7.744145139	2.943690673	-2.630760497	0.008519405	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1435494_c0_g1	8.561253607	-6.832014461	3.065864849	-2.228413449	0.025852957	NA	down_regulated
stress	under	CvsDi	TRINITY_DN97047_c0_g1	14.5324743	-5.112433527	2.424185637	-2.10892823	0.034950777	NA	down_regulated
stress	under	CvsDi	TRINITY_DN63454_c0_g1	26.24501865	-4.987555945	2.51853936	-1.980336708	0.047665707	0.28279715	down_regulated
stress	under	CvsDr	TRINITY_DN1347375_c0_g1	58.0631615	-9.698819018	2.967463673	-3.268386773	0.001081624	0.030659893	down_regulated
stress	under	CvsDr	TRINITY_DN27084_c0_g1	89.82854523	-8.725935652	2.790234166	-3.127313026	0.00176412	0.037147325	down_regulated
stress	under	CvsDr	TRINITY_DN63454_c0_g1	28.67977363	-8.681706568	3.086062688	-2.813198384	0.004905138	0.076607612	down_regulated
stress	under	CvsDr	TRINITY_DN73884_c0_g1	24.81422029	-8.472215915	3.017472552	-2.807719298	0.00498937	0.076607612	down_regulated
stress	under	CvsDr	TRINITY_DN7611_c0_g1	23.99674225	-8.423953175	2.998540163	-2.809351457	0.004964142	NA	down_regulated
stress	under	CvsDr	TRINITY_DN184957_c1_g1	16.13316728	-7.850925466	3.072474369	-2.555245227	0.010611301	NA	down_regulated
stress	under	CvsDr	TRINITY_DN16267_c0_g1	13.65094365	-7.610112593	3.05717017	-2.489266927	0.012800682	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1303_c0_g1	138.2078849	-7.18335267	2.586768114	-2.776960421	0.005486986	0.076888617	down_regulated
stress	under	CvsDr	TRINITY_DN1435494_c0_g1	8.434720924	-6.914864159	3.229452978	-2.141187441	0.032258925	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1699202_c0_g1	8.413096491	-6.912908558	3.616544735	-1.911467731	0.0559445	NA	down_regulated
stress	under	CvsDr	TRINITY_DN9247_c5_g1	7.481903779	-6.743000157	3.184865494	-2.117200921	0.034242799	NA	down_regulated
stress	under	CvsDr	TRINITY_DN80219_c0_g1	7.316775156	-6.710174374	3.200231435	-2.096777846	0.036013246	NA	down_regulated
stress	under	CvsDr	TRINITY_DN2940_c0_g1	6.991961297	-6.646086967	3.263411153	-2.036546011	0.041695558	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1303_c1_g1	6.361236317	-6.508229402	3.243911976	-2.006290384	0.044825275	NA	down_regulated
stress	under	CvsDr	TRINITY_DN85_c0_g4	352.0758734	-6.401627663	3.234090119	-1.979421546	0.047768565	0.236278068	down_regulated
stress	under	CvsDr	TRINITY_DN164716_c0_g1	5.4327653	-6.280371115	3.308837186	-1.898059881	0.057688194	NA	down_regulated
stress	under	CvsDr	TRINITY_DN87655_c0_g1	13.99684381	-6.270551939	3.041126248	-2.061917667	0.039215575	NA	down_regulated
stress	under	CvsDr	TRINITY_DN21835_c1_g1	38.27676914	-5.127207658	2.572886107	-1.99278454	0.046285045	0.236241366	down_regulated
stress	under	CvsDxD	TRINITY_DN11881_c1_g1	6.206463833	-6.340600409	3.152005283	-2.011608433	0.04426123	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN49379_c2_g2	5.19891918	-6.083611486	3.230374499	-1.883252697	0.059666131	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN6552_c0_g1	16.05463101	-5.14345254	2.623760297	-1.960336295	0.049956496	NA	down_regulated
stress	under	CvsDi	TRINITY_DN24680_c0_g1	26.79655125	4.339927724	2.195310793	1.976908115	0.048052017	0.28336129	up_regulated
stress	under	CvsDi	TRINITY_DN41875_c0_g1	52.66580299	4.517510231	2.316132018	1.950454549	0.051121966	0.292598074	up_regulated
stress	under	CvsDi	TRINITY_DN253328_c0_g1	16.08923488	4.692683831	2.358962744	1.989299679	0.046668135	NA	up_regulated
stress	under	CvsDi	TRINITY_DN843_c6_g1	106.615102	4.756368863	2.170374421	2.191496922	0.028415852	0.197490173	up_regulated
stress	under	CvsDi	TRINITY_DN1303_c2_g1	29.97454907	5.484982923	2.545984014	2.1543666	0.031211434	0.208004966	up_regulated
stress	under	CvsDi	TRINITY_DN14454_c1_g1	17.21488525	5.748756245	2.731308937	2.10476236	0.035311999	0.227540233	up_regulated
stress	under	CvsDi	TRINITY_DN56437_c0_g1	6.33920456	5.850551617	3.081344161	1.898701122	0.057603784	NA	up_regulated
stress	under	CvsDi	TRINITY_DN20844_c0_g1	19.53681411	5.938200116	2.653877906	2.237555881	0.025250031	0.186123337	up_regulated
stress	under	CvsDi	TRINITY_DN5345_c0_g1	6.977259537	5.988831679	3.052979748	1.961634918	0.049805004	NA	up_regulated
stress	under	CvsDi	TRINITY_DN87808_c0_g1	7.256750796	6.045912138	3.063746055	1.973372476	0.048453141	NA	up_regulated
stress	under	CvsDi	TRINITY_DN156821_c0_g1	7.317114406	6.057675332	3.024800028	2.002669689	0.045212754	NA	up_regulated
stress	under	CvsDi	TRINITY_DN89139_c1_g1	22.14970719	6.103303141	3.100928095	1.968218209	0.049042936	0.287462511	up_regulated
stress	under	CvsDi	TRINITY_DN10265_c0_g3	8.276005843	6.234967866	3.067946937	2.032293255	0.042123977	NA	up_regulated
stress	under	CvsDi	TRINITY_DN2712_c0_g2	24.55101807	6.254174918	3.095164046	2.02062793	0.043318295	0.263429384	up_regulated
stress	under	CvsDi	TRINITY_DN77526_c0_g2	8.46102418	6.266914433	3.023631446	2.072644945	0.038205335	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1180_c0_g1	9.355934062	6.412259035	2.943212249	2.17866008	0.029356929	NA	up_regulated
stress	under	CvsDi	TRINITY_DN988601_c0_g1	9.412679727	6.42122596	3.142915936	2.043079131	0.041044605	NA	up_regulated
stress	under	CvsDi	TRINITY_DN49379_c2_g1	9.435316081	6.424678435	3.035614103	2.116434506	0.03430787	NA	up_regulated
stress	under	CvsDi	TRINITY_DN74872_c0_g1	9.642970772	6.456071758	3.014878582	2.14140357	0.032241507	NA	up_regulated
stress	under	CvsDi	TRINITY_DN53745_c1_g1	9.805352756	6.48012345	3.411154303	1.899686403	0.057474286	NA	up_regulated
stress	under	CvsDi	TRINITY_DN38523_c2_g1	10.005462	6.509265734	3.406290242	1.91095452	0.056010426	NA	up_regulated
stress	under	CvsDi	TRINITY_DN36319_c0_g1	10.20557124	6.537830947	3.401631591	1.921969141	0.054609641	NA	up_regulated
stress	under	CvsDi	TRINITY_DN34677_c0_g1	29.95396755	6.545024675	3.08718092	2.120065148	0.034000552	0.222030448	up_regulated
stress	under	CvsDi	TRINITY_DN26604_c0_g2	30.58447707	6.588576772	2.769612072	2.378880725	0.017365294	0.16091839	up_regulated
stress	under	CvsDi	TRINITY_DN2221_c2_g3	10.71142603	6.60756249	2.925355454	2.258721237	0.023900731	NA	up_regulated
stress	under	CvsDi	TRINITY_DN63454_c2_g1	10.80589896	6.620281973	3.388774564	1.953591733	0.050749519	NA	up_regulated
stress	under	CvsDi	TRINITY_DN484720_c0_g1	10.82098986	6.622353252	3.061367905	2.163200719	0.030525745	NA	up_regulated
stress	under	CvsDi	TRINITY_DN15543_c1_g2	11.40622668	6.698274266	3.377390238	1.983269268	0.04733736	NA	up_regulated
stress	under	CvsDi	TRINITY_DN32378_c0_g1	11.40622668	6.698274266	3.377390238	1.983269268	0.04733736	NA	up_regulated
stress	under	CvsDi	TRINITY_DN39434_c3_g1	11.80644515	6.748021014	3.370507798	2.002078446	0.045276295	NA	up_regulated
stress	under	CvsDi	TRINITY_DN33686_c0_g1	12.60688211	6.842646134	3.358196286	2.037595647	0.041590388	NA	up_regulated
stress	under	CvsDi	TRINITY_DN167772_c0_g1	12.76140909	6.859901637	2.891568261	2.372381012	0.017673855	NA	up_regulated
stress	under	CvsDi	TRINITY_DN17669_c0_g2	13.41486453	6.932305197	3.082584628	2.248861275	0.024521323	NA	up_regulated
stress	under	CvsDi	TRINITY_DN14958_c0_g1	14.43050163	7.037586832	2.98812767	2.35518278	0.018513597	NA	up_regulated
stress	under	CvsDi	TRINITY_DN85021_c0_g1	15.40841147	7.13211971	3.326359564	2.144121696	0.032023137	NA	up_regulated
stress	under	CvsDi	TRINITY_DN21835_c0_g1	17.00928539	7.274709388	3.313578768	2.195423709	0.028133218	0.197490173	up_regulated
stress	under	CvsDi	TRINITY_DN12509_c0_g2	17.82481325	7.34231541	2.997940295	2.449119958	0.014320575	0.148233181	up_regulated
stress	under	CvsDi	TRINITY_DN35732_c0_g2	23.03519894	7.712232955	2.950000544	2.614315774	0.008940636	0.119167658	up_regulated
stress	under	CvsDi	TRINITY_DN4062_c0_g1	28.01529359	7.994540312	3.272137138	2.443216765	0.014556991	0.148452241	up_regulated
stress	under	CvsDi	TRINITY_DN7105_c1_g1	31.23213233	8.151383477	2.970116767	2.744465661	0.00606095	0.103461472	up_regulated
stress	under	CvsDi	TRINITY_DN8703_c0_g1	84.24599	22.32872197	3.263664441	6.841610826	7.83E-12	5.44E-10	up_regulated
stress	under	CvsDr	TRINITY_DN4017_c6_g1	36.64366982	5.841128087	2.615815328	2.233004763	0.02554863	0.163733396	up_regulated
stress	under	CvsDr	TRINITY_DN4257_c0_g2	8.315399346	6.169662313	3.155272294	1.955350201	0.05054175	NA	up_regulated
stress	under	CvsDr	TRINITY_DN14958_c0_g1	11.61231597	6.654183287	3.09412016	2.15058981	0.03150859	NA	up_regulated
stress	under	CvsDr	TRINITY_DN26604_c0_g2	35.67869024	6.787963264	2.891966008	2.347179477	0.01891614	0.138031639	up_regulated
stress	under	CvsDr	TRINITY_DN69016_c0_g2	13.61867286	6.880658853	3.583578508	1.920052494	0.054851269	NA	up_regulated
stress	under	CvsDr	TRINITY_DN29_c0_g3	14.83462579	7.004189407	3.574297269	1.959599015	0.050042677	NA	up_regulated
stress	under	CvsDr	TRINITY_DN19321_c0_g1	17.34645766	7.233233191	3.529886195	2.049140621	0.040448366	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1180_c0_g1	17.47985189	7.240891474	3.120422493	2.320484322	0.020314692	NA	up_regulated
stress	under	CvsDr	TRINITY_DN8703_c0_g1	20.90470538	7.502160769	3.517421268	2.132858193	0.032936361	NA	up_regulated
stress	under	CvsDr	TRINITY_DN2831_c0_g3	22.37353398	7.597587456	3.544008768	2.143783482	0.032050239	NA	up_regulated
stress	under	CvsDr	TRINITY_DN30172_c8_g1	25.50077536	7.788640247	3.508449273	2.219966612	0.026421034	0.163947225	up_regulated
stress	under	CvsDr	TRINITY_DN12274_c0_g1	32.02422952	8.117041382	3.502643837	2.317404155	0.020481726	0.141046167	up_regulated
stress	under	CvsDr	TRINITY_DN34347_c2_g2	33.95161371	8.201307814	3.501810055	2.342019608	0.019179707	0.138582787	up_regulated
stress	under	CvsDxD	TRINITY_DN2831_c0_g2	131.7550097	4.489824361	2.336165538	1.921877662	0.054621153	0.232907052	up_regulated
stress	under	CvsDxD	TRINITY_DN6287_c0_g1	21.77835414	4.518593132	2.369664947	1.906848957	0.05654015	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN69787_c0_g1	168.500177	4.703785883	2.141708771	2.196277078	0.028072117	0.139462498	up_regulated
stress	under	CvsDxD	TRINITY_DN1096_c0_g1	187.5833932	4.747680858	2.166795694	2.191106836	0.028444062	0.140188592	up_regulated
stress	under	CvsDxD	TRINITY_DN9125_c0_g1	43.78609675	4.817447007	2.35880209	2.042327768	0.041119029	0.185794821	up_regulated
stress	under	CvsDxD	TRINITY_DN861_c0_g1	182.7439083	5.040600775	2.193406319	2.298069779	0.021557818	0.122107342	up_regulated
stress	under	CvsDxD	TRINITY_DN26096_c0_g1	39.90309749	5.235105507	2.484316842	2.10726161	0.035094909	0.16630566	up_regulated
stress	under	CvsDxD	TRINITY_DN45675_c0_g1	20.63018412	5.260986485	2.597350862	2.025520141	0.042813982	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN7347_c0_g1	201.1203496	5.523059634	2.282580248	2.419656281	0.015535184	0.099995162	up_regulated
stress	under	CvsDxD	TRINITY_DN774_c0_g2	354.2775994	5.586174511	2.313669058	2.414422448	0.015760178	0.100520797	up_regulated
stress	under	CvsDxD	TRINITY_DN4017_c6_g1	31.90693926	5.745866228	2.548344627	2.254744577	0.024149366	0.129729499	up_regulated
stress	under	CvsDxD	TRINITY_DN37896_c0_g2	6.896325709	6.011931571	3.116564337	1.929025337	0.053727717	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN26621_c0_g1	7.015230673	6.037464216	3.191421027	1.891779294	0.058520393	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN252739_c0_g1	7.816809325	6.193394466	3.147839978	1.967506134	0.049124889	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN730_c0_g3	87.24161196	6.288786539	2.323202837	2.706946823	0.006790513	0.067815785	up_regulated
stress	under	CvsDxD	TRINITY_DN29191_c1_g1	8.485736512	6.311920802	3.237002247	1.949927841	0.05118472	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN6046_c0_g3	9.16153695	6.422295153	3.118751654	2.059251863	0.039470117	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN65988_c0_g1	9.421170016	6.460520218	3.076424886	2.100009088	0.035728042	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN89139_c1_g3	27.83926378	6.468843847	2.971467532	2.176986212	0.029481596	0.14436472	up_regulated
stress	under	CvsDxD	TRINITY_DN17349_c4_g1	9.545744843	6.481534289	3.112166129	2.082644056	0.03728368	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN26659_c1_g1	9.878576892	6.528707514	3.223117726	2.025587667	0.042807056	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN18029_c1_g1	9.89678987	6.533650795	3.127918282	2.088817612	0.036724144	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN62845_c0_g2	10.36227338	6.597879645	3.503993911	1.882959792	0.059705817	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1287858_c0_g1	10.56571706	6.627797747	3.490708604	1.898696941	0.057604334	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN2245_c1_g1	12.12437177	6.824558736	3.042346604	2.243189099	0.024884623	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN5384_c2_g2	13.06547514	6.932472926	3.470535658	1.99752246	0.045768457	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN10265_c0_g3	14.59989993	7.094127055	3.44793093	2.057502659	0.039637899	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN44394_c0_g2	15.36831572	7.168094892	3.442808287	2.082048808	0.037338012	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN27766_c0_g1	16.31062247	7.253215086	2.926999276	2.478037882	0.013210712	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN774_c0_g1	164.6885308	7.307768434	2.455419272	2.976179473	0.00291864	0.041797128	up_regulated
stress	under	CvsDxD	TRINITY_DN19634_c1_g1	98.35410775	7.543083487	2.615057912	2.884480474	0.003920601	0.051305792	up_regulated
stress	under	CvsDxD	TRINITY_DN1649245_c0_g1	23.2882238	7.766580484	2.977617781	2.608320159	0.009098781	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1557528_c0_g1	24.32881577	7.829710265	3.42256078	2.287676032	0.022156394	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN123572_c0_g1	25.45514983	7.895019338	3.420855261	2.307908033	0.021004252	0.12077445	up_regulated
stress	under	CvsDxD	TRINITY_DN36234_c0_g2	27.70781796	8.017385859	3.418166603	2.345522261	0.019000444	0.116301104	up_regulated
stress	under	CvsDxD	TRINITY_DN2831_c0_g3	28.05404944	8.036158752	3.007783651	2.671787497	0.007544841	0.070697955	up_regulated
stress	under	CvsDxD	TRINITY_DN153188_c0_g1	28.49565011	8.057826997	2.943197074	2.737780309	0.006185538	0.064459963	up_regulated
stress	under	CvsDxD	TRINITY_DN57237_c4_g1	67.99741457	9.312765168	3.163321818	2.943982846	0.00324018	0.044714478	up_regulated
stress	under	CvsDxD	TRINITY_DN29_c0_g3	117.6901304	10.10449926	2.862337047	3.530157036	0.000415313	0.010869746	up_regulated
stress	under	CvsDxD	TRINITY_DN1180_c0_g1	118.7614784	10.1175849	2.919263769	3.465800182	0.000528656	0.012303439	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


HSP90_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

HSP90_reg

HSP90_reg_final <- HSP90_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

HSP90_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

HSP90_reg_final_plot <- HSP90_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                               panel.background = element_rect(fill = "white"), 
                                                                                               panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                               axis.title.x = element_text(color="black", vjust=1),
                                                                                               axis.title.y = element_text(color="black" , vjust=1)) 






HSP90_reg_final_plot


jpeg("HSP90_regulation_under.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(HSP90_reg_final_plot)
dev.off()





################ multicopper oxidase inter


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
pathogenicity	inter	CvsDi	TRINITY_DN272308_c0_g1	8.331018555	-6.495893412	3.281036695	-1.979829553	0.047722684	0.922241767	down_regulated
pathogenicity	inter	CvsDi	TRINITY_DN489606_c0_g1	31.48351208	-8.414088873	3.619287419	-2.324791567	0.020083108	0.922241767	down_regulated
pathogenicity	inter	CvsDi	TRINITY_DN60385_c0_g1	12.72065102	-7.106274056	3.698842472	-1.921215653	0.054704526	0.922241767	down_regulated
pathogenicity	inter	CvsDr	TRINITY_DN489606_c0_g1	30.71572017	-8.272514627	3.713506905	-2.227682576	0.02590169	0.908093125	down_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN489606_c0_g1	26.35234141	-7.852050846	3.978745308	-1.973499241	0.048438711	0.935664183	down_regulated
pathogenicity	inter	CvsDi	TRINITY_DN90163_c0_g1	28.44800137	-8.267564648	3.626562347	-2.279724945	0.022624007	0.922241767	down_regulated
pathogenicity	inter	CvsDr	TRINITY_DN12594_c0_g1	163.9984872	-5.235477196	2.520546482	-2.077119876	0.037790497	0.908093125	down_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN573_c0_g1	2080.39405	27.0559407	4.113625948	6.57715141	4.80E-11	4.63E-08	up_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN703318_c0_g1	3552.365812	28.11129911	4.139043116	6.791738652	1.11E-11	2.14E-08	up_regulated
pathogenicity	inter	CvsDr	TRINITY_DN46989_c0_g1	11.23563	7.2021914	3.758638513	1.916170277	0.055343425	0.908093125	up_regulated
pathogenicity	inter	CvsDr	TRINITY_DN161346_c0_g1	11.56665154	7.245000746	3.288543526	2.203103194	0.027587476	0.908093125	up_regulated
pathogenicity	inter	CvsDr	TRINITY_DN336788_c0_g1	15.1681005	7.635262394	3.728418214	2.047855674	0.040574143	0.908093125	up_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN1309481_c0_g1	16.51965816	7.905073683	3.971286423	1.990557427	0.046529565	0.935664183	up_regulated
pathogenicity	inter	CvsDi	TRINITY_DN236959_c0_g1	13.17594609	7.321560971	3.677294272	1.991018512	0.046478852	0.922241767	up_regulated
pathogenicity	inter	CvsDi	TRINITY_DN175061_c0_g1	17.34595341	7.716668201	3.126849478	2.467873256	0.013591844	0.922241767	up_regulated
pathogenicity	inter	CvsDr	TRINITY_DN63077_c0_g1	14.98543899	6.163036194	3.028048992	2.035315878	0.041819099	0.908093125	up_regulated
pathogenicity	inter	CvsDr	TRINITY_DN17739_c0_g1	33.8677991	6.358301035	2.813969737	2.259548478	0.023849288	0.908093125	up_regulated
pathogenicity	inter	CvsDr	TRINITY_DN390315_c0_g1	7.281739462	6.576158968	3.495109213	1.881531754	0.05989962	0.908093125	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


multicop_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

multicop_reg

multicop_reg_final <- multicop_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

multicop_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

multicop_reg_final_plot <- multicop_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                               panel.background = element_rect(fill = "white"), 
                                                                                               panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                               axis.title.x = element_text(color="black", vjust=1),
                                                                                               axis.title.y = element_text(color="black" , vjust=1)) 






multicop_reg_final_plot


jpeg("multicop_regulation_inter.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(multicop_reg_final_plot)
dev.off()


################ multicopper oxidase under


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
pathogenicity	under	CvsDr	TRINITY_DN274300_c0_g1	8.952857532	-6.98885114	3.340973843	-2.091860478	0.036450998	0.962938997	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN274300_c0_g1	7.490268348	-6.67291218	3.230515955	-2.06558713	0.038867481	NA	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN20609_c0_g1	170.8692578	-11.24329304	3.186876958	-3.527997218	0.000418717	0.609441949	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN85730_c0_g1	10.56181395	-7.227281113	3.370311516	-2.144395579	0.032001204	0.962938997	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN85730_c0_g2	10.56181395	0	3.370311516	-2.144395579	0.032001204	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN61429_c0_g1	40.99286317	6.992959793	3.486839932	2.005529342	0.044906487	0.962938997	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN1815177_c0_g1	20.56133201	7.492396183	3.166052392	2.3664789	0.017958198	0.979901061	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN22846_c0_g1	24.7660196	5.24134204	2.748446514	1.907019843	0.056518018	0.24578766	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN178883_c0_g2	10.59879275	6.634168927	3.222642203	2.058611695	0.039531452	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN331557_c0_g1	11.57356589	6.756616452	3.323892662	2.03274207	0.042078589	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN102333_c0_g2	16.40270926	7.263140516	3.098712928	2.343921714	0.019082176	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN22778_c0_g1	18.42503618	7.430816388	3.568085913	2.082577766	0.037289728	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN963324_c0_g1	103.6524933	22.65802965	3.60653927	6.282485218	3.33E-10	1.32E-08	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


multicop_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

multicop_reg

multicop_reg_final <- multicop_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

multicop_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

multicop_reg_final_plot <- multicop_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                                     panel.background = element_rect(fill = "white"), 
                                                                                                     panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                                     axis.title.x = element_text(color="black", vjust=1),
                                                                                                     axis.title.y = element_text(color="black" , vjust=1)) 






multicop_reg_final_plot


jpeg("multicop_regulation_under.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(multicop_reg_final_plot)
dev.off()






################ phage encoded virulence factor inter


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation	
pathogenicity	inter	CvsDi	TRINITY_DN1341961_c0_g1	114.8605372	-23.68761099	3.628972978	-6.527359431	6.69E-11	9.30E-08	down_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN1324060_c0_g1	90.54223818	-23.25197269	3.622127163	-6.419424731	1.37E-10	9.30E-08	down_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN1341961_c0_g1	112.0594205	-23.54015608	3.736940142	-6.299313125	2.99E-10	4.21E-07	down_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN1324060_c0_g1	88.33417455	-23.22001062	3.728809123	-6.227192076	4.75E-10	4.21E-07	down_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN2076_c0_g1	126.7983656	-10.31805494	3.292309747	-3.133986696	0.001724487	0.611503001	down_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN1341961_c0_g1	96.14061108	-23.15112627	4.004279614	-5.781595819	7.40E-09	2.38E-06	down_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN2076_c0_g1	108.9729892	-9.899955411	3.527309107	-2.806659442	0.005005813	0.935664183	down_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN1324060_c0_g1	75.78569909	-9.375948921	3.996154429	-2.346242891	0.018963745	0.935664183	down_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN1324060_c0_g2	75.78569909	0	3.996154429	-2.346242891	0.018963745	0.935664183	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN1324060_c0_g3	75.78569909	0	3.996154429	-2.346242891	0.018963745	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN1324060_c0_g4	75.78569909	0	3.996154429	-2.346242891	0.018963745	0.935664183	up_regulated	
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


phage_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

phage_reg

phage_reg_final <- phage_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

phage_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

phage_reg_final_plot <- phage_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                                     panel.background = element_rect(fill = "white"), 
                                                                                                     panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                                     axis.title.x = element_text(color="black", vjust=1),
                                                                                                     axis.title.y = element_text(color="black" , vjust=1)) 






phage_reg_final_plot


jpeg("phage_regulation_inter.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(phage_reg_final_plot)
dev.off()




################ phage encoded virulence factor under


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
pathogenicity	under	CvsDi	TRINITY_DN50605_c0_g2	29.69039921	-7.268407411	3.256960364	-2.231653627	0.025637864	0.979901061	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN50605_c0_g2	33.95004363	-8.91185527	3.690899842	-2.414548118	0.015754742	0.962938997	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN50605_c0_g2	27.29191466	-8.536817222	3.540596644	-2.41112391	0.015903444	0.135088704	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN23423_c1_g1	16.26397435	7.248308015	3.5979745	2.014552359	0.043951574	NA	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN23423_c1_g2	16.26397435	0	3.5979745	2.014552359	0.043951574	NA	up_regulated
pathogenicity	under	CvsDr	TRINITY_DN23423_c1_g3	16.26397435	0	3.5979745	2.014552359	0.043951574	NA	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


phage_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

phage_reg

phage_reg_final <- phage_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

phage_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

phage_reg_final_plot <- phage_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                               panel.background = element_rect(fill = "white"), 
                                                                                               panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                               axis.title.x = element_text(color="black", vjust=1),
                                                                                               axis.title.y = element_text(color="black" , vjust=1)) 






phage_reg_final_plot


jpeg("phage_regulation_under.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(phage_reg_final_plot)
dev.off()




################ stress induced bacterial acidophilic repeat motif inter


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	inter	CvsDr	TRINITY_DN7618_c0_g2	39.9786291	-8.639979882	3.57570401	-2.416301757	0.015679061	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN1309107_c0_g1	39.12400629	-8.608838255	3.575292063	-2.407869932	0.016045897	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN566591_c0_g1	22.76707711	-7.827757599	3.592534017	-2.178895888	0.029339403	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN485541_c0_g1	22.10395836	-7.785114698	3.594292559	-2.165965783	0.030313802	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN27960_c0_g1	97.2094991	-6.222110295	3.213534719	-1.936220031	0.052840758	0.293673665	down_regulated
stress	inter	CvsDi	TRINITY_DN7618_c0_g2	41.4809314	-8.984522229	3.485146942	-2.577946462	0.00993894	0.097685577	down_regulated
stress	inter	CvsDi	TRINITY_DN566591_c0_g1	26.79610512	-8.353591778	3.509833204	-2.380053778	0.017310112	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN485541_c0_g1	26.01563604	-8.310938868	3.511752183	-2.366607447	0.017951963	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN351352_c0_g1	22.7102306	-8.115589633	3.514876696	-2.308925842	0.020947697	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN27960_c0_g1	114.7735982	-5.92239275	3.14048556	-1.885820723	0.059319117	0.385014649	down_regulated
stress	inter	CvsDxD	TRINITY_DN1309107_c0_g1	39.11302411	-8.386364511	3.897064702	-2.151969534	0.031399754	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN7618_c0_g2	33.78384916	-8.175462764	3.88847163	-2.10248744	0.035510599	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN566591_c0_g1	22.76068635	-7.605141914	3.904793987	-1.947642293	0.051457779	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN485541_c0_g1	22.09775373	-7.562488912	3.905845935	-1.936197443	0.052843524	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN747053_c0_g1	10.15688274	6.956220375	3.680658491	1.889939094	0.058766106	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN67032_c0_g1	11.57394393	7.146470066	3.285216487	2.175342201	0.029604482	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN12263_c0_g3	23.33387584	8.157118034	3.046128766	2.677863826	0.007409333	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN711967_c0_g1	19.12381264	7.725562512	3.543668133	2.180103278	0.029249807	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN12263_c0_g3	149.2382999	10.68998572	3.251630081	3.287577446	0.001010534	0.021726479	up_regulated
stress	inter	CvsDxD	TRINITY_DN105079_c0_g1	9.435590265	6.972032102	3.552567713	1.962533206	0.049700439	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN36780_c0_g1	17.63524961	7.874504693	3.890045121	2.024270786	0.042942298	NA	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


stress_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

stress_reg

stress_reg_final <- stress_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

stress_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

stress_reg_final_plot <- stress_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                               panel.background = element_rect(fill = "white"), 
                                                                                               panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                               axis.title.x = element_text(color="black", vjust=1),
                                                                                               axis.title.y = element_text(color="black" , vjust=1)) 






stress_reg_final_plot


jpeg("stress_regulation_inter.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(stress_reg_final_plot)
dev.off()



################ stress induced bacterial acidophilic repeat motif under


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	under	CvsDi	TRINITY_DN2117_c1_g1	17.90340205	-7.897780133	3.293604616	-2.397913853	0.016488743	0.157289673	down_regulated
stress	under	CvsDi	TRINITY_DN188268_c0_g1	8.446428976	-6.818075505	3.422310011	-1.992243684	0.046344327	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1884_c2_g1	28.87197008	-8.691331623	3.207216748	-2.709929607	0.006729749	0.086631267	down_regulated
stress	under	CvsDr	TRINITY_DN486859_c0_g1	20.13133803	-8.171151729	3.502683136	-2.332826411	0.019657258	NA	down_regulated
stress	under	CvsDr	TRINITY_DN2117_c1_g1	17.35938433	-7.956579407	3.518519145	-2.261343218	0.023738012	NA	down_regulated
stress	under	CvsDr	TRINITY_DN188268_c0_g1	9.614967418	-7.105457238	3.589330097	-1.979605399	0.047747886	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN2117_c1_g1	17.76753788	-7.857459742	3.424688813	-2.294357289	0.021769984	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN9117_c0_g1	57.30051175	-4.681386509	2.464769689	-1.899320058	0.057522407	0.235997335	down_regulated
stress	under	CvsDi	TRINITY_DN378_c0_g1	453.1525997	4.326035006	2.234279527	1.936210288	0.052841951	0.298925688	up_regulated
stress	under	CvsDi	TRINITY_DN172039_c0_g1	7.619242019	6.116316148	3.139061246	1.948453907	0.051360675	NA	up_regulated
stress	under	CvsDi	TRINITY_DN11833_c0_g1	79.22954906	6.303939659	2.467417848	2.55487317	0.01062265	0.132698631	up_regulated
stress	under	CvsDi	TRINITY_DN512091_c0_g1	10.08453445	6.520230922	2.948807268	2.21114177	0.027026021	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1767260_c0_g1	15.82341129	7.170217701	2.838713729	2.52586854	0.011541266	NA	up_regulated
stress	under	CvsDi	TRINITY_DN89700_c0_g1	15.91395671	7.178383684	2.890113012	2.48377266	0.012999877	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1575170_c0_g1	22.81245335	7.698169932	3.285078453	2.34337476	0.019110177	0.164412176	up_regulated
stress	under	CvsDr	TRINITY_DN26743_c0_g1	98.09311748	6.634918845	3.166962645	2.095041713	0.036167285	0.204115944	up_regulated
stress	under	CvsDr	TRINITY_DN11210_c0_g2	16.75341637	7.183099647	3.532704296	2.03331472	0.042020737	NA	up_regulated
stress	under	CvsDr	TRINITY_DN4502_c0_g1	48.23608114	7.221189215	3.333009819	2.166567039	0.030267883	0.175649053	up_regulated
stress	under	CvsDr	TRINITY_DN24552_c0_g2	18.53254023	7.328556149	3.524972747	2.079039095	0.037613757	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN4502_c0_g1	24.8686994	6.313318136	2.763546174	2.284498879	0.022342228	0.124689348	up_regulated
stress	under	CvsDxD	TRINITY_DN331980_c0_g2	9.996278469	6.547941065	3.076819566	2.12815244	0.033324447	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1828000_c0_g1	30.07045584	6.569664231	3.214504195	2.043756621	0.040977597	0.185794821	up_regulated
stress	under	CvsDxD	TRINITY_DN947180_c0_g1	13.06547514	6.932472926	3.470535658	1.99752246	0.045768457	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN54397_c0_g2	43.5864646	7.110327517	3.214298739	2.212092931	0.026960244	0.138018561	up_regulated
stress	under	CvsDxD	TRINITY_DN139075_c0_g1	16.49464978	7.270220007	3.038039762	2.393062822	0.01670838	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN48852_c0_g1	17.57081139	7.360074233	3.441273353	2.138764776	0.032454723	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN679476_c0_g1	26.5814839	7.957499755	3.41940223	2.327161071	0.019956694	0.11833696	up_regulated
stress	under	CvsDxD	TRINITY_DN1877249_c0_g1	26.80675071	7.969677688	3.419138725	2.330902115	0.019758522	0.11833696	up_regulated
stress	under	CvsDxD	TRINITY_DN948028_c0_g1	166.2415805	7.98382832	3.138989655	2.543438876	0.010976726	0.088322486	up_regulated
stress	under	CvsDxD	TRINITY_DN5365_c0_g1	66.22844293	9.274746408	3.418529173	2.713080959	0.006666082	0.067460748	up_regulated
stress	under	CvsDxD	TRINITY_DN1551842_c0_g1	200.8481127	10.8755535	3.122754634	3.482679486	0.000496422	0.01215434	up_regulated
stress	under	CvsDxD	TRINITY_DN49322_c0_g1	102.271133	22.64254555	3.43101851	6.599365606	4.13E-11	2.13E-09	up_regulated
stress	under	CvsDxD	TRINITY_DN105788_c0_g1	103.8480007	22.66441406	3.431542282	6.604731108	3.98E-11	2.13E-09	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


stress_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

stress_reg

stress_reg_final <- stress_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

stress_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

stress_reg_final_plot <- stress_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                                 panel.background = element_rect(fill = "white"), 
                                                                                                 panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                                 axis.title.x = element_text(color="black", vjust=1),
                                                                                                 axis.title.y = element_text(color="black" , vjust=1)) 






stress_reg_final_plot


jpeg("stress_regulation_under.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(stress_reg_final_plot)
dev.off()




################ universal stress protein family inter


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	inter	CvsDr	TRINITY_DN19054_c0_g1	15.69381043	-7.291023643	3.622908417	-2.012478044	0.044169569	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN80569_c0_g1	23.95316853	-6.398480778	3.003381604	-2.130425507	0.0331365	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN26273_c0_g1	7.760999803	-6.567145452	3.295741799	-1.992615275	0.046303591	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN26273_c0_g2	7.760999803	0	3.295741799	-1.992615275	0.046303591	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN125600_c0_g1	7.006525183	6.422647002	3.353553028	1.915176813	0.055469958	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN422100_c0_g1	7.1513182	6.44972392	3.356032916	1.921829756	0.054627183	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN1370168_c0_g1	7.64204865	6.546609828	3.259968338	2.008182028	0.044623952	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN615905_c0_g1	58.86017656	9.347882612	3.488596297	2.679554129	0.007372028	0.089270477	up_regulated
stress	inter	CvsDxD	TRINITY_DN538056_c0_g1	11.68644921	7.280789898	3.480102009	2.092119679	0.036427811	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1295788_c0_g1	12.73656917	7.40494191	3.91659018	1.890660388	0.058669693	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN25140_c0_g1	13.33693866	7.471395412	3.488674534	2.141614341	0.032224529	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN806387_c0_g1	15.34919874	7.674175903	3.89964222	1.967917945	0.04907748	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN615905_c0_g1	31.24244759	8.699957058	3.486611367	2.495247144	0.012586943	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN67427_c0_g2	37.07792751	8.946928388	3.866957419	2.313686813	0.020684908	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN67427_c0_g1	38.0048757	8.982549256	3.866969963	2.322890879	0.020185015	NA	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


universal_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

universal_reg

universal_reg_final <- universal_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

universal_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

universal_reg_final_plot <- universal_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                                 panel.background = element_rect(fill = "white"), 
                                                                                                 panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                                 axis.title.x = element_text(color="black", vjust=1),
                                                                                                 axis.title.y = element_text(color="black" , vjust=1)) 






universal_reg_final_plot


jpeg("universal_regulation_inter.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(universal_reg_final_plot)
dev.off()



################ universal stress protein family under


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	under	CvsDi	TRINITY_DN956523_c0_g1	31.67410866	-8.723083183	3.236219141	-2.695455037	0.007029256	0.112025547	down_regulated
stress	under	CvsDr	TRINITY_DN956523_c0_g1	36.05612782	-9.01176896	3.483910097	-2.586682408	0.009690486	0.099501893	down_regulated
stress	under	CvsDr	TRINITY_DN943950_c0_g1	27.3425636	-8.6127495	3.489026499	-2.468525103	0.013567114	0.113624576	down_regulated
stress	under	CvsDr	TRINITY_DN691487_c0_g1	25.9214284	-8.535856857	3.125049985	-2.731430505	0.006306004	0.082991515	down_regulated
stress	under	CvsDr	TRINITY_DN148458_c0_g2	17.85204851	-7.996952009	3.093819416	-2.58481538	0.009743115	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1294576_c0_g1	13.95685477	-7.642901104	3.11925664	-2.450231573	0.014276436	NA	down_regulated
stress	under	CvsDr	TRINITY_DN167334_c0_g1	10.92510963	-7.289706596	3.162390419	-2.305125436	0.021159549	NA	down_regulated
stress	under	CvsDr	TRINITY_DN47134_c0_g1	9.942502971	-7.153899302	3.279514419	-2.1813898	0.029154597	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1357530_c0_g1	9.281988476	-7.053326148	3.148810988	-2.239996676	0.025091139	NA	down_regulated
stress	under	CvsDr	TRINITY_DN400333_c0_g1	6.637357922	-6.571217731	3.357187862	-1.957357765	0.05030542	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1294576_c0_g1	12.00351551	-7.295239577	3.041758457	-2.398362552	0.016468556	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN725545_c0_g1	11.83681471	-7.27533308	3.079891361	-2.362204451	0.01816662	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN943950_c0_g1	23.14690766	-6.727068723	3.194301599	-2.105959163	0.0352079	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1598275_c0_g1	48.71078633	5.230164273	2.493814702	2.097254566	0.035971046	0.230262028	up_regulated
stress	under	CvsDi	TRINITY_DN1259300_c0_g1	7.056641556	6.005570516	3.072331595	1.954727324	0.050615263	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1311228_c0_g1	9.605243516	6.450380361	3.416236512	1.88815392	0.05900529	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1557260_c0_g1	12.40677287	6.819565337	3.361107267	2.028963909	0.042461967	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1557260_c0_g2	12.40677287	0	3.361107267	2.028963909	0.042461967	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN947240_c0_g1	12.51424953	6.87123436	2.969597314	2.313860646	0.020675367	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN136480_c0_g1	13.25517231	6.954789296	3.45873192	2.01079166	0.044347467	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1251170_c0_g1	13.6393802	6.995992086	3.455375598	2.024669067	0.042901357	NA	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


universal_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

universal_reg

universal_reg_final <- universal_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

universal_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

universal_reg_final_plot <- universal_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                                       panel.background = element_rect(fill = "white"), 
                                                                                                       panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                                       axis.title.x = element_text(color="black", vjust=1),
                                                                                                       axis.title.y = element_text(color="black" , vjust=1)) 






universal_reg_final_plot


jpeg("universal_regulation_under.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(universal_reg_final_plot)
dev.off()




################ viral superfamily 1 RNA helicase inter


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	inter	CvsDr	TRINITY_DN3820_c0_g1	2900.260376	-27.97272959	3.740756587	-7.477826729	7.56E-14	2.14E-11	down_regulated
stress	inter	CvsDi	TRINITY_DN3820_c0_g1	3413.511604	-28.47488287	3.654349805	-7.79205177	6.59E-15	2.27E-12	down_regulated
stress	inter	CvsDxD	TRINITY_DN3820_c0_g1	2899.446268	-27.76029911	4.066164192	-6.827146619	8.66E-12	2.49E-09	down_regulated
stress	inter	CvsDi	TRINITY_DN374486_c0_g1	15.02262004	0	3.901346816	1.959103952	0.050100614	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN374486_c0_g1	15.02262004	0	3.901346816	1.959103952	0.050100614	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN374486_c0_g1	15.02262004	7.643143963	3.901346816	1.959103952	0.050100614	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN419470_c0_g1	15.44913646	7.684060541	3.895928655	1.972330918	0.048571842	NA	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


viral_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

viral_reg

viral_reg_final <- viral_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

viral_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

viral_reg_final_plot <- viral_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                                       panel.background = element_rect(fill = "white"), 
                                                                                                       panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                                       axis.title.x = element_text(color="black", vjust=1),
                                                                                                       axis.title.y = element_text(color="black" , vjust=1)) 






viral_reg_final_plot


jpeg("viral_regulation_inter.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(viral_reg_final_plot)
dev.off()




################ viral superfamily 1 RNA helicase under


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	under	CvsDi	TRINITY_DN36423_c1_g1	222.7745642	-24.50895715	3.268619509	-7.498259458	6.47E-14	8.52E-12	down_regulated
stress	under	CvsDi	TRINITY_DN22654_c2_g1	169.9843831	-24.48420508	3.257322829	-7.516665176	5.62E-14	8.52E-12	down_regulated
stress	under	CvsDi	TRINITY_DN12396_c0_g1	53.84598472	-9.488268872	3.227683593	-2.939652726	0.003285803	0.077756635	down_regulated
stress	under	CvsDr	TRINITY_DN36423_c1_g1	253.5947657	-25.03677942	3.539145212	-7.074244747	1.50E-12	7.45E-10	down_regulated
stress	under	CvsDr	TRINITY_DN22654_c2_g1	193.5012193	-24.80378717	3.526814848	-7.032914467	2.02E-12	7.45E-10	down_regulated
stress	under	CvsDr	TRINITY_DN12396_c0_g1	62.18497922	-6.075840439	3.106665119	-1.955743605	0.050495365	0.24483608	down_regulated
stress	under	CvsDxD	TRINITY_DN36423_c1_g1	212.5919217	-24.77041642	3.428611582	-7.22462018	5.03E-13	7.63E-11	down_regulated
stress	under	CvsDxD	TRINITY_DN22654_c2_g1	162.2146891	-24.40188101	3.416475834	-7.142412883	9.17E-13	1.16E-10	down_regulated
stress	under	CvsDxD	TRINITY_DN12396_c0_g1	51.6100441	-7.89095053	3.192019037	-2.472087553	0.013432661	0.094401759	down_regulated
stress	under	CvsDr	TRINITY_DN1777829_c0_g1	60.79764669	9.040557137	3.533486073	2.558537645	0.010511344	0.099505535	up_regulated
stress	under	CvsDi	TRINITY_DN1777829_c0_g2	60.79764669	0	3.533486073	2.558537645	0.010511344	0.099505535	up_regulated
stress	under	CvsDxD	TRINITY_DN432385_c0_g1	24.13550844	7.818347376	2.90303628	2.693162132	0.007077784	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN55261_c5_g1	43.25122804	8.659960037	3.413065598	2.537296688	0.011171224	0.088322486	up_regulated
stress	under	CvsDxD	TRINITY_DN1757990_c0_g1	96.41419583	22.55547818	3.429048777	6.577765336	4.78E-11	2.13E-09	up_regulated
")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


viral_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

viral_reg

viral_reg_final <- viral_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

viral_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

viral_reg_final_plot <- viral_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                               panel.background = element_rect(fill = "white"), 
                                                                                               panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                               axis.title.x = element_text(color="black", vjust=1),
                                                                                               axis.title.y = element_text(color="black" , vjust=1)) 






viral_reg_final_plot


jpeg("viral_regulation_under.jpeg",height=6,width=6,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(viral_reg_final_plot)
dev.off()





###Facet grid  

################1,3 beta glucan synthase 

Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	inter	CvsDi	TRINITY_DN103194_c0_g2	10.62458061	6.878436828	3.636470911	1.891514327	0.05855572	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN713865_c0_g1	61.91721377	-9.562063664	3.489260667	-2.740426863	0.006135944	0.084430584	down_regulated
stress	inter	CvsDi	TRINITY_DN123998_c0_g1	11.12337825	-7.086181154	3.619747842	-1.957644969	0.050271687	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN199459_c0_g1	11.92593303	7.188434478	3.156014802	2.277693525	0.022744844	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN713865_c0_g1	52.60742089	-9.036038536	3.574937871	-2.527607153	0.011484277	0.09027918	down_regulated
stress	inter	CvsDr	TRINITY_DN36313_c0_g1	29.13814435	5.423208905	2.698208867	2.009929243	0.044438678	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN103194_c0_g2	19.15692921	7.994348427	3.882197129	2.059232996	0.039471924	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1119070_c0_g1	22.86050876	8.248946442	3.877830892	2.127206336	0.033402943	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN713865_c0_g1	52.59265389	-8.813615764	3.899802371	-2.260016002	0.023820258	0.105175599	down_regulated
stress	under	CvsDr	TRINITY_DN85759_c0_g1	18.98360275	-8.086592809	3.165304557	-2.554759792	0.01062611	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN85759_c0_g1	16.03552908	-7.712882824	3.083260828	-2.501534334	0.012365645	NA	down_regulated
stress	under	CvsDi	TRINITY_DN236596_c0_g1	157.1169518	6.04618856	2.557785607	2.363837119	0.018086763	0.162153655	up_regulated
stress	under	CvsDi	TRINITY_DN47653_c0_g1	12.01409985	6.77322341	3.098469383	2.185990104	0.028816329	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN40509_c0_g1	77.03511704	5.202573716	2.714657294	1.916475324	0.055304621	0.233201153	up_regulated
stress	under	CvsDxD	TRINITY_DN66074_c0_g1	7.757356843	6.182042438	3.103828906	1.99174717	0.046398805	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN12574_c1_g1	15.17621177	7.149955455	3.444026926	2.076045167	0.037889774	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN36266_c0_g1	110.6518732	21.44038983	3.424434225	6.261002087	3.82511E-10	1.61292E-08	up_regulated
stress	under	CvsDi	TRINITY_DN12574_c1_g1	15.17621177	0	3.444026926	2.076045167	0.037889774	NA	down_regulated
stress	under	CvsDr	TRINITY_DN36266_c0_g1	110.6518732	0	3.424434225	6.261002087	3.82511E-10	1.61292E-08	up_regulated")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


beta_glucan_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

beta_glucan_reg

beta_glucan_reg_final <- beta_glucan_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

beta_glucan_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

beta_glucan_reg_final_plot <- beta_glucan_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                                           panel.background = element_rect(fill = "white"), 
                                                                                                           panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                                           axis.title.x = element_text(color="black", vjust=1),
                                                                                                           axis.title.y = element_text(color="black" , vjust=1)) 






beta_glucan_reg_final_plot


beta_glucan_reg_final_plot_facet <- beta_glucan_reg_final_plot + facet_grid( ~ location, scales='free_y') #scales='fixed' makes that all y-axes are the same

beta_glucan_reg_final_plot_facet


jpeg("beta_glucan_regulation_FACET.jpeg",height=6,width=10,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(beta_glucan_reg_final_plot_facet)
dev.off()



################ Calcineurin-like phosphoesterase facet


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
pathogenicity	inter	CvsDi	TRINITY_DN98454_c0_g2	36.47744848	-8.626477067	3.615734566	-2.385815914	0.017041279	0.922241767	down_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN32202_c0_g1	18.89010725	-7.677212401	3.648065482	-2.104461238	0.035338232	0.922241767	down_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN38671_c0_g1	18.38967582	-7.638418306	3.125246656	-2.444100945	0.014521363	0.922241767	down_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN119546_c0_g1	10.67217798	-6.853073656	3.230738545	-2.121209612	0.033904167	0.922241767	down_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN57481_c1_g1	8.071420202	-6.450301354	3.28828688	-1.961599334	0.04980915	0.922241767	down_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN8146_c1_g3	6.666546839	6.337544826	3.352641308	1.890314007	0.058715977	0.922241767	up_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN368727_c0_g1	7.746044454	6.553782838	3.302312389	1.98460414	0.047188531	0.922241767	up_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN529783_c0_g1	7.898609819	6.582189911	3.296479704	1.996733031	0.045854192	0.922241767	up_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN1335698_c0_g1	9.052099007	6.780516805	3.321536031	2.041379874	0.041213083	0.922241767	up_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN590150_c0_g1	9.618406845	6.865899623	3.24212203	2.117717828	0.034198972	0.922241767	up_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN1161203_c0_g1	12.51714879	7.247623521	3.685151608	1.966709729	0.049216685	0.922241767	up_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN10358_c0_g1	13.0580009	7.306601122	3.183370746	2.295240393	0.021719351	0.922241767	up_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN185248_c0_g1	14.10746056	7.418638822	3.158538382	2.348756901	0.0188362	0.922241767	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN30001_c0_g1	35.37603633	-8.476315674	3.712647627	-2.283091887	0.022424956	0.908093125	down_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN2726_c1_g1	20.0870204	-7.660000165	3.722329824	-2.057851004	0.039604438	0.908093125	down_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN98454_c0_g2	35.86875963	-7.010560538	3.487564638	-2.010159313	0.04441433	0.908093125	down_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN15468_c1_g1	10.1296611	-6.672294225	3.265517751	-2.043257681	0.041026937	0.908093125	down_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN24355_c0_g1	7.436540209	6.607690793	3.347607994	1.973854407	0.0483983	0.908093125	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN66000_c0_g1	7.824531157	6.680687601	3.321782477	2.01117552	0.044306921	0.908093125	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN2726_c0_g2	61.19016108	7.21958157	2.877534922	2.508946639	0.012109178	0.908093125	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN49517_c0_g1	17.82949796	7.868692221	3.199775674	2.45913871	0.01392708	0.908093125	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN20889_c0_g1	32.4338388	8.732237912	3.694531426	2.363557622	0.018100412	0.908093125	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN104820_c1_g1	34.03305107	8.801346782	3.223766215	2.730144246	0.006330662	0.908093125	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN44345_c0_g1	64.13120141	9.715504691	3.253033197	2.986598692	0.002820998	0.833604812	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN4475_c0_g3	88.85444789	10.1859604	3.174285645	3.208898486	0.001332445	0.590606367	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN201050_c0_g1	14.606319	7.58080268	3.731483693	2.03157867	0.042196328	0.908093125	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN30001_c0_g1	30.35062769	-8.05582964	3.978467757	-2.02485734	0.042882015	0.935664183	down_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN1285991_c0_g1	7.851305129	6.834279715	3.542022823	1.929484946	0.053670687	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN4475_c0_g3	9.843476532	7.160361764	3.506856821	2.041817539	0.041169634	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN2642_c1_g3	57.44307103	7.216304666	3.224494283	2.237964788	0.025223351	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN153672_c0_g1	12.65335519	7.520111226	3.986402009	1.886440758	0.059235585	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN104025_c0_g1	12.85206629	7.54525539	3.97091866	1.900128418	0.057416269	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN103195_c1_g1	14.05928354	7.672241235	3.979485569	1.927947998	0.053861595	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN550537_c0_g1	15.03965204	7.771837703	3.961386198	1.961898516	0.049774301	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN104820_c1_g2	15.31310026	7.797812895	3.960468822	1.968911572	0.048963246	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN86380_c1_g1	15.4652119	7.809849218	3.97431303	1.965081552	0.049404799	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN103258_c0_g1	16.87114025	7.93546801	3.970410062	1.998652	0.04564602	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN137115_c0_g1	16.87114025	7.93546801	3.970410062	1.998652	0.04564602	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN33837_c0_g3	16.87114025	7.93546801	3.970410062	1.998652	0.04564602	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN36035_c0_g1	19.68299696	8.157999162	3.965216351	2.057390679	0.039648661	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN53847_c0_g1	22.49485367	8.350748841	3.962289829	2.107556287	0.035069388	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN681044_c0_g1	25.30671038	8.520755426	3.96076691	2.151289288	0.031453373	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN924584_c0_g1	29.25895943	8.7313837	3.945978916	2.212729436	0.026916305	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN1335698_c0_g1	32.26688984	8.872502437	3.946220029	2.248354722	0.02455358	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN21079_c0_g2	33.58621276	8.929176499	3.570640775	2.500721036	0.012394076	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN13423_c0_g1	92.79127138	23.08398068	3.984695311	5.7931608	6.91E-09	2.38E-06	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN28695_c0_g1	21.79188949	8.304921446	3.962864288	2.095686565	0.036110004	0.935664183	up_regulated	
pathogenicity	under	CvsDi	TRINITY_DN66019_c0_g1	98.30746204	-23.79520984	3.488236208	-6.821559213	9.01E-12	8.59E-09	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN3452_c0_g1	101.6705614	-10.44029468	2.898839406	-3.601542967	0.000316334	0.190622945	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN35261_c9_g1	44.28013163	-9.241477742	3.467839818	-2.664909058	0.007700915	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN104605_c3_g1	13.76165442	-7.555677483	3.031745077	-2.492187598	0.012695896	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN112598_c0_g1	13.6460783	-7.544323177	3.506047946	-2.151802626	0.031412903	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN355870_c0_g1	8.911724605	-6.930302615	3.562961754	-1.945095989	0.051763427	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN19532_c0_g1	8.40914851	-6.845257323	3.121910453	-2.192650118	0.028332597	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN64002_c0_g1	6.955522112	-6.57277621	3.201992792	-2.052714243	0.0401003	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN6863_c1_g1	28.00012472	-5.707242845	2.698542874	-2.114935027	0.034435485	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN20996_c0_g2	11.20226798	-5.685842184	2.871282019	-1.980245112	0.047675993	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN89230_c2_g1	7.571307622	6.052250298	3.204542169	1.88864742	0.058939088	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN101609_c0_g1	8.314532214	6.187100327	3.173387662	1.94968311	0.051213901	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN135423_c0_g1	27.14730058	6.356858122	3.039818956	2.09119629	0.036510472	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN147556_c0_g1	9.491087514	6.37815028	3.220339936	1.980582922	0.047638066	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN29050_c1_g1	10.00202188	6.453709859	3.213948031	2.008031803	0.044639912	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN122061_c2_g1	10.68326769	6.548665332	3.206517498	2.042298331	0.041121948	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN10875_c0_g1	10.76087134	6.558839482	3.126101599	2.098089033	0.035897281	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN122283_c0_g1	14.87907526	7.026104684	3.233057584	2.173207405	0.029764711	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN62789_c0_g2	50.99090412	7.274108334	3.050994987	2.384175774	0.017117424	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN67675_c0_g1	18.22332556	7.3181432	3.507415989	2.086477117	0.036935426	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN105673_c0_g1	21.45924318	7.553808468	3.499619916	2.15846539	0.030891666	0.979901061	up_regulated
pathogenicity	under	CvsDr	TRINITY_DN66019_c0_g1	113.060051	-23.7903463	3.719613141	-6.395919522	1.60E-10	4.65E-07	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN325546_c0_g1	17.28776598	-7.93820016	3.339070409	-2.377368305	0.017436668	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN70643_c2_g1	16.61327245	-7.880813019	3.224583615	-2.443978498	0.014526292	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN313471_c0_g1	14.04723538	-7.638769362	3.23070297	-2.364429486	0.018057865	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN35261_c3_g1	13.43305593	-7.574238586	3.286617695	-2.304569405	0.021190701	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN20608_c0_g1	9.276910593	-7.040139797	3.355799737	-2.097902243	0.035913782	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN105676_c0_g1	8.632574102	-6.936292471	3.345907052	-2.073067889	0.038165962	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN34153_c0_g1	55.78338475	-6.903807205	2.843808665	-2.427662343	0.015196484	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN167609_c0_g1	7.679263071	-6.767441498	3.411224545	-1.983874532	0.047269829	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN36911_c0_g1	7.65287566	-6.762566835	3.322584644	-2.035333199	0.041817358	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN138058_c0_g1	7.362749271	-6.706703487	3.469517418	-1.933036408	0.053231713	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN1263207_c0_g1	6.710873519	-6.572995897	3.38778437	-1.940204919	0.052354791	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN90005_c2_g1	6.669407587	-6.564242703	3.366258916	-1.950011234	0.05117478	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN31408_c0_g1	6.398129349	-6.504110231	3.44304477	-1.889057699	0.058884098	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN326873_c0_g2	6.390590089	-6.502443817	3.397719181	-1.913767287	0.055649897	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN77552_c0_g1	6.356663417	-6.494933638	3.370546928	-1.926967277	0.053983708	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN162086_c0_g1	6.066537028	-6.427387572	3.396993225	-1.892081363	0.058480141	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN36911_c3_g3	16.54613319	-6.245698807	3.030371893	-2.06103377	0.039299818	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN39450_c0_g1	10.03908377	-5.766427317	3.048645101	-1.891472154	0.058561345	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN122061_c1_g2	29.71891893	-5.535232308	2.852900004	-1.94021252	0.052353868	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN48752_c0_g1	26.40747943	-4.892818987	2.559781748	-1.911420375	0.05595058	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN144986_c0_g1	21.55039673	7.524406721	3.711745363	2.027188286	0.042643161	0.962938997	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1416171_c0_g1	9.643918779	-7.036424901	3.146573702	-2.236218048	0.025337492	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1354525_c0_g1	8.309339412	-6.8230494	3.316997751	-2.056995486	0.03968666	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1557454_c0_g1	6.085984662	-6.372651508	3.249146782	-1.961330754	0.049840452	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1263207_c0_g1	5.687972097	-6.275907161	3.286107407	-1.909830198	0.05615508	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN313471_c0_g1	12.33647858	-5.992554864	2.907944889	-2.060752557	0.039326653	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN11751_c3_g1	9.715111261	-5.641913778	2.931311213	-1.924706511	0.054266085	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN24346_c0_g1	51.61033726	5.593099691	2.696351205	2.074321654	0.038049448	0.173542605	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN162455_c0_g1	8.193318648	6.262360497	3.187606548	1.964596447	0.049460964	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN166549_c0_g1	8.215303401	6.263094676	3.171076885	1.975068692	0.048260353	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN690171_c0_g1	24.51060172	6.28643624	3.33929329	1.88256487	0.059759361	0.253977284	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN947496_c0_g1	12.33819387	6.852840741	3.594555912	1.906449895	0.05659186	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN468039_c0_g1	12.86553195	6.909810627	3.614645518	1.91161501	0.055925593	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1312601_c0_g1	14.55080438	7.088144507	3.085194742	2.297470695	0.021591933	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN983507_c0_g1	14.64132339	7.099492764	3.58083155	1.982638017	0.047407878	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1551617_c0_g1	14.83585338	7.118276684	3.098986034	2.296969591	0.021620505	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN130680_c0_g1	16.77994367	7.296000424	3.572560275	2.042232982	0.041128426	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1008324_c0_g1	19.25940801	7.493620103	3.038051189	2.466587835	0.013640727	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN38753_c0_g1	21.35572517	7.641749094	3.061195117	2.496328657	0.012548628	0.135088704	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN122061_c3_g2	21.87169448	7.678273443	3.219094615	2.385227637	0.017068556	0.135088704	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN81237_c1_g3	22.44345887	7.715475507	3.188865447	2.419504878	0.015541652	0.135088704	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN116547_c0_g3	23.03129523	7.752507547	3.560817849	2.177170491	0.029467849	0.145012836	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN216128_c0_g1	78.00096922	7.988688114	2.805253596	2.847759692	0.004402816	0.077935997	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN95525_c0_g1	35.20497985	8.364354092	3.557430696	2.351234587	0.018711233	0.135088704	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1587001_c0_g1	37.33556413	8.449252492	3.222246703	2.622161886	0.008737392	0.10892615	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN4387_c2_g1	38.73533076	8.501362205	2.998857049	2.834867439	0.00458447	0.077935997	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN956653_c0_g1	16.61543441	7.281799236	3.573086141	2.03795793	0.04155414	NA	up_regulated
   ")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


Calcineurin_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

Calcineurin_reg

Calcineurin_reg_final <- Calcineurin_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

Calcineurin_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

Calcineurin_reg_final_plot <- Calcineurin_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                                           panel.background = element_rect(fill = "white"), 
                                                                                                           panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                                           axis.title.x = element_text(color="black", vjust=1),
                                                                                                           axis.title.y = element_text(color="black" , vjust=1)) 






Calcineurin_reg_final_plot

Calcineurin_reg_final_plot_facet <- Calcineurin_reg_final_plot + facet_grid( ~ location, scales='free_y') #scales='fixed' makes that all y-axes are the same

Calcineurin_reg_final_plot_facet

jpeg("Calcineurin_regulation_FACET.jpeg",height=6,width=10,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(Calcineurin_reg_final_plot_facet)
dev.off()


################Calcineurin-like phosphoesterase under


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation

")

Data = read.table(textConnection(Input),header=TRUE)




################ CLP amino terminal 


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
pathogenicity	inter	CvsDi	TRINITY_DN929082_c0_g1	55.04572622	-9.219915643	3.617355647	-2.548799881	0.010809431	0.922241767	down_regulated
pathogenicity	inter	CvsDi	TRINITY_DN30866_c0_g1	23.1995726	-7.973683229	3.149815359	-2.531476395	0.011358346	0.922241767	down_regulated
pathogenicity	inter	CvsDi	TRINITY_DN26131_c0_g1	20.35304163	-7.784438141	3.645735213	-2.135217641	0.032743237	0.922241767	down_regulated
pathogenicity	inter	CvsDi	TRINITY_DN1325222_c0_g1	14.33042619	-7.278726834	3.678018321	-1.978980581	0.047818193	0.922241767	down_regulated
pathogenicity	inter	CvsDi	TRINITY_DN24844_c2_g1	7.563441139	6.520629102	3.323219388	1.96214223	0.049745927	0.922241767	up_regulated
pathogenicity	inter	CvsDi	TRINITY_DN83095_c0_g2	8.25923423	6.648640399	3.389794924	1.961369507	0.049835934	0.922241767	up_regulated
pathogenicity	inter	CvsDi	TRINITY_DN7990_c0_g1	9.137630636	6.794305137	3.368005835	2.017308007	0.043663381	0.922241767	up_regulated
pathogenicity	inter	CvsDi	TRINITY_DN241587_c0_g1	11.41915328	7.115291864	3.700591234	1.922744614	0.054512131	0.922241767	up_regulated
pathogenicity	inter	CvsDi	TRINITY_DN52430_c0_g1	22.04764839	8.062399764	3.113792116	2.589254345	0.009618402	0.922241767	up_regulated
pathogenicity	inter	CvsDi	TRINITY_DN27643_c1_g1	66.94073019	9.665566952	3.189722345	3.03022204	0.00244374	0.806434231	up_regulated
pathogenicity	inter	CvsDi	TRINITY_DN68002_c2_g1	111.5563436	23.09517202	3.615931293	6.387060525	1.69E-10	9.30E-08	up_regulated
pathogenicity	inter	CvsDr	TRINITY_DN929082_c0_g1	48.29000865	-8.925371406	3.712102415	-2.404397942	0.016199129	0.908093125	down_regulated
pathogenicity	inter	CvsDr	TRINITY_DN26635_c0_g1	6.505757565	6.414276102	3.37106402	1.902745265	0.057073792	0.908093125	up_regulated
pathogenicity	inter	CvsDr	TRINITY_DN15798_c0_g1	15.1681005	7.635262394	3.728418214	2.047855674	0.040574143	0.908093125	up_regulated
pathogenicity	inter	CvsDr	TRINITY_DN4731_c0_g1	15.19195078	7.637621493	3.237352508	2.359218366	0.018313476	0.908093125	up_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN929082_c0_g1	45.88547158	-8.651994365	3.986245599	-2.170461942	0.029971869	0.935664183	down_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN30866_c0_g1	19.40819585	-7.410870215	3.464643241	-2.138999516	0.032435708	0.935664183	down_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN8760_c0_g1	16.71311361	6.440702745	3.253271194	1.979762018	0.047730276	0.935664183	up_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN36847_c0_g1	8.905092045	7.013996916	3.506176119	2.00046908	0.045449635	0.935664183	up_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN265795_c0_g1	10.03823153	7.187678745	3.482597652	2.063884337	0.039028684	0.935664183	up_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN24844_c2_g1	13.74780742	7.640335857	3.460877426	2.207629718	0.027270096	0.935664183	up_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN43276_c0_g1	17.22262234	7.965235349	3.969592393	2.006562529	0.044796265	0.935664183	up_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN88021_c0_g1	23.90078202	8.43825483	3.961389965	2.130124755	0.033161315	0.935664183	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN780055_c0_g2	14.2033659	-7.599220825	3.516117741	-2.16125323	0.030675784	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN280474_c1_g1	11.97512994	-7.356066597	3.519985421	-2.089800302	0.036635742	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN1175922_c0_g1	11.77840099	-7.328871734	3.535783445	-2.072771664	0.038193535	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN53697_c1_g1	6.941996641	-6.567118476	3.16791166	-2.07301187	0.038171175	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN64340_c0_g1	26.88099128	-5.87989635	2.701567764	-2.176475611	0.029519715	0.979901061	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN37175_c0_g1	8.407760746	6.202153624	3.120665467	1.987445848	0.046873011	0.979901061	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN79300_c1_g1	9.150464606	6.325484598	3.224504469	1.961691993	0.049798355	0.979901061	up_regulated
pathogenicity	under	CvsDr	TRINITY_DN280474_c1_g1	13.77218751	-7.61018164	3.721434699	-2.04495907	0.040858893	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN780055_c0_g2	12.97706581	-7.524532652	3.725755768	-2.019599008	0.043424998	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN1175922_c0_g1	10.76146921	-7.254461454	3.745069639	-1.937069842	0.052736807	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN1669707_c0_g1	9.273140963	-7.039562251	3.336461538	-2.109888626	0.03486795	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN394943_c0_g1	6.680716478	-6.566635889	3.351762716	-1.959158939	0.050094176	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN32304_c1_g1	6.398129349	-6.504110231	3.44304477	-1.889057699	0.058884098	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN6403_c0_g2	24.91396401	7.731472525	3.280551647	2.356759886	0.018435163	0.962938997	up_regulated
pathogenicity	under	CvsDr	TRINITY_DN467012_c0_g1	26.84371575	7.8393521	3.734154368	2.099364763	0.035784758	0.962938997	up_regulated
pathogenicity	under	CvsDr	TRINITY_DN4210_c0_g1	102.3710007	8.329843541	3.54067187	2.352616635	0.018641842	0.962938997	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN780055_c0_g2	13.43741089	-7.512422757	3.585364027	-2.095302653	0.036144097	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1175922_c0_g1	11.14321879	-7.242059594	3.603237124	-2.009875938	0.044444321	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN280474_c1_g1	11.0712484	-7.236196521	3.588900025	-2.016271412	0.043771603	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1891971_c0_g1	9.268952148	-6.980166876	3.611382643	-1.932823953	0.053257889	NA	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1384041_c0_g1	56.38291531	5.139801973	2.548273533	2.016974201	0.043698206	0.194561059	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN4210_c0_g1	37.96821642	6.938841007	2.944688847	2.35639192	0.018453437	0.135088704	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN6403_c0_g2	18.71554116	7.45085989	3.138969499	2.373664316	0.017612554	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN999723_c0_g1	20.87614618	7.608765212	3.587019083	2.121194517	0.033905436	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN40606_c0_g1	21.31554525	7.63872848	3.11163968	2.454888504	0.014092829	0.135088704	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN794616_c0_g1	39.32483351	8.522825862	3.5813036	2.379811045	0.017321518	0.135088704	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN7519_c0_g1	120.1592135	22.85697927	3.61228638	6.327565663	2.49E-10	1.32E-08	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN1823270_c0_g1	129.626303	22.92259682	3.615356437	6.340342154	2.29E-10	1.32E-08	up_regulated
   ")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


clp_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

clp_reg

clp_reg_final <- clp_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

clp_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

clp_reg_final_plot <- clp_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                           panel.background = element_rect(fill = "white"), 
                                                                                           panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                           axis.title.x = element_text(color="black", vjust=1),
                                                                                           axis.title.y = element_text(color="black" , vjust=1)) 






clp_reg_final_plot


clp_reg_final_plot_facet <- clp_reg_final_plot + facet_grid( ~ location, scales='free_y') #scales='fixed' makes that all y-axes are the same

clp_reg_final_plot_facet

jpeg("clpaminoterminal_regulation_FACET.jpeg",height=6,width=10,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(clp_reg_final_plot_facet)
dev.off()





################ heat shock protein 9/12


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	inter	CvsDi	TRINITY_DN16682_c0_g1	376.7064098	-25.45947281	3.547096753	-7.1775524	7.10E-13	8.14E-11	down_regulated	
stress	inter	CvsDi	TRINITY_DN3381_c0_g1	51.0061871	-9.282782652	3.05121737	-3.04232099	0.002347614	0.04250417	down_regulated	
stress	inter	CvsDi	TRINITY_DN5386_c2_g1	50.96900271	-9.28171139	2.981297497	-3.11331271	0.001849999	0.035355534	down_regulated	
stress	inter	CvsDi	TRINITY_DN81460_c0_g1	36.36347094	-7.360326152	3.291361675	-2.236255653	0.02533503	0.202680236	down_regulated	
stress	inter	CvsDi	TRINITY_DN27148_c0_g1	12.7520442	-7.282072704	3.138583637	-2.320177999	0.02033125	NA	down_regulated	
stress	inter	CvsDi	TRINITY_DN236523_c0_g1	11.38353461	-7.119662824	3.310177794	-2.150840005	0.03148883	NA	down_regulated	
stress	inter	CvsDi	TRINITY_DN26649_c0_g1	166.1415633	-6.031810915	2.851591225	-2.11524389	0.034409166	0.246599023	down_regulated	
stress	inter	CvsDi	TRINITY_DN1162041_c0_g1	11.66086137	7.011699	3.621378282	1.936196237	0.052843671	NA	up_regulated	
stress	inter	CvsDi	TRINITY_DN23708_c0_g1	142.9621604	23.30854619	3.512128767	6.63658645	3.21E-11	1.00E-09	up_regulated	
stress	inter	CvsDr	TRINITY_DN16682_c0_g1	320.065317	-24.93120513	3.633918878	-6.860693912	6.85E-12	5.39E-10	down_regulated	
stress	inter	CvsDr	TRINITY_DN26649_c0_g1	139.0338981	-23.82152017	3.598694261	-6.619489861	3.60E-11	1.46E-09	down_regulated	
stress	inter	CvsDr	TRINITY_DN29285_c0_g1	88.19479384	-23.20637161	3.583997941	-6.4749958	9.48E-11	2.98E-09	down_regulated	
stress	inter	CvsDr	TRINITY_DN5386_c3_g1	36.69257087	-8.516274828	3.576081078	-2.381454627	0.017244415	NA	down_regulated	
stress	inter	CvsDr	TRINITY_DN81460_c0_g1	34.84171028	-8.441567412	3.57754767	-2.359596067	0.018294843	NA	down_regulated	
stress	inter	CvsDr	TRINITY_DN3403_c0_g1	13.54957969	7.372358378	3.157228655	2.33507268	0.019539624	NA	up_regulated	
stress	inter	CvsDr	TRINITY_DN2097_c0_g2	43.81969412	9.065986734	3.566843599	2.541739351	0.01103024	0.09027918	up_regulated	
stress	inter	CvsDxD	TRINITY_DN16682_c0_g1	319.9754741	-24.74932824	3.962666286	-6.245625157	4.22E-10	2.85E-08	down_regulated	
stress	inter	CvsDxD	TRINITY_DN29285_c0_g1	88.1700374	-22.99189894	3.911967375	-5.877323794	4.17E-09	5.57E-08	down_regulated	
stress	inter	CvsDxD	TRINITY_DN5386_c3_g1	36.6822712	-8.293787909	3.897016697	-2.128240281	0.033317167	NA	down_regulated	
stress	inter	CvsDxD	TRINITY_DN81460_c0_g1	29.44290764	-7.977085436	3.88979504	-2.05077269	0.040289088	NA	down_regulated	
stress	inter	CvsDxD	TRINITY_DN26649_c0_g1	140.2308019	-6.809810775	3.539035683	-1.924199524	0.054329579	0.202501157	down_regulated	
stress	inter	CvsDxD	TRINITY_DN5386_c2_g1	42.38277967	-6.005706001	3.01554624	-1.991581466	0.046416999	NA	down_regulated	
stress	inter	CvsDxD	TRINITY_DN76258_c0_g2	15.02262004	7.643143963	3.901346816	1.959103952	0.050100614	NA	up_regulated	
stress	inter	CvsDxD	TRINITY_DN1385446_c0_g1	22.53393006	8.228185597	3.878335919	2.121576307	0.033873334	NA	up_regulated	
stress	inter	CvsDxD	TRINITY_DN1367435_c0_g1	40.82233707	9.085513452	3.870322517	2.347482261	0.018900773	NA	up_regulated	
stress	inter	CvsDxD	TRINITY_DN781459_c0_g1	50.36418487	9.388729913	3.869230517	2.426510872	0.015244794	0.082986683	up_regulated	
stress	under	CvsDi	TRINITY_DN448268_c0_g1	35.44327047	-8.885368154	2.988445563	-2.973240759	0.002946732	0.073517189	down_regulated	
stress	under	CvsDi	TRINITY_DN730953_c0_g1	161.8104893	-7.649079337	2.906641318	-2.631586942	0.008498712	0.117778564	down_regulated	
stress	under	CvsDi	TRINITY_DN1249926_c0_g1	10.29408531	-7.103079678	3.370480105	-2.107438542	0.035079584	NA	down_regulated	
stress	under	CvsDi	TRINITY_DN109666_c1_g1	7.431600851	-6.627747144	3.478476679	-1.905359086	0.05673341	NA	down_regulated	
stress	under	CvsDi	TRINITY_DN10633_c1_g2	19.34817783	5.904652457	3.110438761	1.898334258	0.057652064	0.315143023	up_regulated	
stress	under	CvsDi	TRINITY_DN118_c2_g1	23.46498011	7.738632717	3.28536622	2.355485568	0.018498516	0.162153655	up_regulated	
stress	under	CvsDi	TRINITY_DN1756643_c0_g1	50.6276377	8.848201479	3.257922663	2.71590286	0.006609531	0.107184558	up_regulated	
stress	under	CvsDr	TRINITY_DN15593_c0_g1	72.1934591	-10.01332341	3.139716086	-3.189244867	0.00142645	0.033912697	down_regulated	
stress	under	CvsDr	TRINITY_DN448268_c0_g1	40.28974388	-9.171974862	3.207213443	-2.85979559	0.004239142	0.072656914	down_regulated	
stress	under	CvsDr	TRINITY_DN1249926_c0_g1	11.71824154	-7.390731985	3.55656	-2.07805632	0.037704172	NA	down_regulated	
stress	under	CvsDr	TRINITY_DN10469_c0_g1	6.225897209	-6.477952995	3.243481869	-1.997221892	0.045801084	NA	down_regulated	
stress	under	CvsDr	TRINITY_DN52451_c0_g1	15.15477861	-5.552727495	2.814064032	-1.973205809	0.048472119	NA	down_regulated	
stress	under	CvsDr	TRINITY_DN2704_c0_g1	28.98972305	-4.490587594	2.364785753	-1.898940565	0.057572291	0.263545208	down_regulated	
stress	under	CvsDr	TRINITY_DN2140_c0_g1	257.4527138	5.72539005	2.90505112	1.97083969	0.048742215	0.239003112	up_regulated	
stress	under	CvsDr	TRINITY_DN118_c0_g1	134.9949779	6.123898612	2.45873361	2.49067186	0.012750182	0.113215468	up_regulated	
stress	under	CvsDr	TRINITY_DN291582_c0_g1	18.68080055	7.340040483	3.524418805	2.082624367	0.037285476	NA	up_regulated	
stress	under	CvsDr	TRINITY_DN1040281_c0_g1	24.75947375	7.74611211	3.509531417	2.207164202	0.02730259	0.164934497	up_regulated	
stress	under	CvsDr	TRINITY_DN93229_c0_g1	112.6853998	8.45650911	2.972841878	2.844587589	0.004446896	0.072830271	up_regulated	
stress	under	CvsDr	TRINITY_DN1849316_c0_g1	51.29807136	8.796446973	3.501966728	2.511859094	0.012009703	0.107940868	up_regulated	
stress	under	CvsDr	TRINITY_DN3292_c0_g1	133.2684415	22.890394	3.55626539	6.436638297	1.22E-10	8.34E-09	up_regulated	
stress	under	CvsDxD	TRINITY_DN730953_c0_g1	153.6505595	-24.31082103	3.414147918	-7.12061153	1.07E-12	1.17E-10	down_regulated	
stress	under	CvsDxD	TRINITY_DN448268_c0_g1	33.83609626	-8.789192257	3.115740374	-2.820900076	0.004788912	0.059571706	down_regulated	
stress	under	CvsDxD	TRINITY_DN6446_c0_g1	15.08564537	-7.621154227	3.439100036	-2.216031563	0.026689341	NA	down_regulated	
stress	under	CvsDxD	TRINITY_DN68171_c0_g1	16.73611391	-5.205193076	2.705118083	-1.924201797	0.054329294	NA	down_regulated	
stress	under	CvsDxD	TRINITY_DN15593_c0_g1	62.50486486	-5.0818996	2.698028867	-1.883560129	0.059624499	0.240718058	down_regulated	
stress	under	CvsDxD	TRINITY_DN10469_c0_g1	153.6053338	4.695824525	2.462429414	1.90698848	0.056522079	0.234081154	up_regulated	
stress	under	CvsDxD	TRINITY_DN121361_c0_g1	7.851175578	6.197073711	3.26190732	1.899831326	0.057455258	NA	up_regulated	
stress	under	CvsDxD	TRINITY_DN13962_c0_g1	55.51906216	6.554794759	2.705989631	2.422328114	0.015421419	0.099995162	up_regulated	
stress	under	CvsDxD	TRINITY_DN34028_c0_g1	37.50426066	6.891682177	3.213676417	2.144485406	0.031994014	0.153692761	up_regulated	
stress	under	CvsDxD	TRINITY_DN16698_c0_g1	29.50995246	8.108315837	3.416570441	2.37323245	0.017633163	0.109701398	up_regulated	
stress	under	CvsDxD	TRINITY_DN1859834_c0_g1	33.33948828	8.284384122	3.414370433	2.42632845	0.015252459	0.099995162	up_regulated	
stress	under	CvsDxD	TRINITY_DN26368_c0_g1	37.16902409	8.441283054	3.413307341	2.473050977	0.013396503	0.094401759	up_regulated	
stress	under	CvsDxD	TRINITY_DN29692_c0_g1	39.91793183	8.544215704	2.936096939	2.91005913	0.003613604	0.048977238	up_regulated	
stress	under	CvsDxD	TRINITY_DN108196_c0_g1	47.04760099	8.781329474	3.154370618	2.783861041	0.005371604	0.063141349	up_regulated	
stress	under	CvsDxD	TRINITY_DN3132_c3_g1	96.63946265	22.5633403	3.429125197	6.579911494	4.71E-11	2.13E-09	up_regulated	

")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


HSP912_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

HSP912_reg

HSP912_reg_final <- HSP912_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

HSP912_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

HSP912_reg_final_plot <- HSP912_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                                 panel.background = element_rect(fill = "white"), 
                                                                                                 panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                                 axis.title.x = element_text(color="black", vjust=1),
                                                                                                 axis.title.y = element_text(color="black" , vjust=1)) 






HSP912_reg_final_plot

HSP912_reg_final_plot_facet <- HSP912_reg_final_plot + facet_grid( ~ location, scales='free_y') #scales='fixed' makes that all y-axes are the same

HSP912_reg_final_plot_facet

jpeg("HSP912_regulation_FACET.jpeg",height=6,width=10,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(HSP912_reg_final_plot_facet)
dev.off()




################ heat shock protein 20 alpha crystallin family 


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	inter	CvsDi	TRINITY_DN725520_c0_g1	497.3077027	-25.83959131	3.553406011	-7.271781279	3.55E-13	6.10E-11	down_regulated
stress	inter	CvsDi	TRINITY_DN143_c0_g2	219.9184575	-24.72184942	3.516899072	-7.029445233	2.07E-12	1.78E-10	down_regulated
stress	inter	CvsDi	TRINITY_DN31886_c0_g1	126.5284276	-23.96637171	3.496553524	-6.854284239	7.17E-12	4.93E-10	down_regulated
stress	inter	CvsDi	TRINITY_DN363_c4_g1	82.46956624	-23.38338876	3.49252917	-6.695259401	2.15E-11	8.23E-10	down_regulated
stress	inter	CvsDi	TRINITY_DN1351665_c0_g1	78.30706447	-23.30056704	3.491676706	-6.673174238	2.50E-11	8.61E-10	down_regulated
stress	inter	CvsDi	TRINITY_DN1174_c0_g2	57.42470378	-9.453524779	2.907833537	-3.251054319	0.001149779	0.023266114	down_regulated
stress	inter	CvsDi	TRINITY_DN5729_c0_g1	54.77520102	-9.385611421	3.131380108	-2.997276312	0.002724037	0.046853434	down_regulated
stress	inter	CvsDi	TRINITY_DN34378_c0_g1	52.55158479	-9.325437155	3.489223794	-2.672639448	0.007525709	0.089270477	down_regulated
stress	inter	CvsDi	TRINITY_DN38017_c0_g1	48.43304281	-9.208032987	3.482845579	-2.643824648	0.008197511	0.09096593	down_regulated
stress	inter	CvsDi	TRINITY_DN2160_c0_g1	244.1914929	-9.151058736	3.217422044	-2.844220811	0.004452018	0.0729283	down_regulated
stress	inter	CvsDi	TRINITY_DN55012_c0_g1	42.40787959	-9.016401409	3.484711224	-2.5874171	0.009669846	0.097685577	down_regulated
stress	inter	CvsDi	TRINITY_DN31824_c1_g1	36.94220317	-8.816914139	3.495097378	-2.522651928	0.011647362	0.10636438	down_regulated
stress	inter	CvsDi	TRINITY_DN690442_c0_g1	29.08067373	-8.471608276	3.173856624	-2.669184302	0.007603572	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN35861_c0_g2	24.9750106	-8.252033297	3.514578667	-2.34794383	0.018877368	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1460036_c0_g1	24.45469787	-8.221653358	3.516118673	-2.338275275	0.019372973	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN347341_c0_g1	19.25157067	-7.876437266	3.537948058	-2.226272725	0.025995919	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN6830_c0_g1	19.1969905	-7.873163868	3.046939407	-2.583958135	0.009767365	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN122091_c0_g1	17.38027852	-7.729805802	3.542852633	-2.181802802	0.02912409	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN492544_c0_g1	17.03048168	-7.700592903	3.148985687	-2.445420103	0.01446835	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN10353_c0_g1	16.91016342	-7.689300503	3.553539797	-2.163842518	0.030476438	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN26986_c0_g1	16.51229304	-7.655036706	3.074270688	-2.490033404	0.012773109	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN19398_c0_g1	16.3898507	-7.64419928	3.557743289	-2.148609009	0.031665407	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1346004_c0_g1	15.60938162	-7.573788584	3.564674295	-2.124678991	0.033613408	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN16579_c0_g2	15.49358029	-7.564136509	3.120626893	-2.423915697	0.015354169	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN53805_c0_g1	15.34922526	-7.549533487	3.567169017	-2.116393546	0.03431135	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN45156_c0_g2	15.06290805	-7.5234216	3.562880173	-2.111612301	0.034719717	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN2999_c0_g1	13.3619974	-7.349653313	3.100465053	-2.370500291	0.01776403	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN16109_c0_g2	13.1630622	-7.328223572	3.086335837	-2.374408995	0.017577069	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN3363_c1_g2	12.4875053	-7.251751739	3.602650542	-2.012893467	0.044125838	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN626_c0_g1	12.27329841	-7.227740342	3.107403983	-2.32597383	0.020019947	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1345002_c0_g1	12.142091	-7.211187653	3.222611227	-2.237684643	0.025241627	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN23371_c0_g1	11.22829043	-7.099135153	3.121011873	-2.274626128	0.022928367	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN221_c0_g2	855.3545927	-7.055955579	2.904331508	-2.429459433	0.015121357	0.126871874	down_regulated
stress	inter	CvsDi	TRINITY_DN39775_c0_g1	10.65990416	-7.0248065	3.629538088	-1.935454686	0.052934524	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN784030_c0_g1	10.40625441	-6.988614206	3.642420386	-1.918673153	0.055025711	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN184502_c0_g1	10.25326869	-6.968859949	3.27604526	-2.127217238	0.033402038	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN38674_c0_g1	10.14609805	-6.952072957	3.648644748	-1.905384995	0.056730044	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN84137_c0_g1	10.14609805	-6.952072957	3.648644748	-1.905384995	0.056730044	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1223037_c0_g1	9.964693018	-6.927551664	3.64607466	-1.900002691	0.057432767	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN17808_c0_g1	55.15637753	-6.885820002	3.162684452	-2.177207403	0.029465096	0.22278813	down_regulated
stress	inter	CvsDi	TRINITY_DN13238_c0_g1	23.36640768	-6.716942528	3.315675686	-2.025814092	0.042783839	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN2999_c0_g2	7.922750604	-6.595124766	3.282152614	-2.009390038	0.044495786	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN50525_c0_g2	7.63417493	-6.541655822	3.281608444	-1.993429726	0.046214412	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN801383_c0_g1	7.057023583	-6.428405247	3.290637656	-1.953543939	0.050755176	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN593_c0_g2	90.7485589	-6.341938763	2.442910696	-2.596058371	0.009430004	0.097685577	down_regulated
stress	inter	CvsDi	TRINITY_DN112586_c0_g1	15.64153958	-6.083061281	2.984905157	-2.037941228	0.041555811	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN14273_c0_g1	47.02288622	-5.427182101	2.633105462	-2.061133585	0.039290297	0.275833925	down_regulated
stress	inter	CvsDi	TRINITY_DN21868_c0_g1	21.09623177	-4.503203472	2.391883869	-1.882701552	0.059740825	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN3206_c0_g1	51.46093204	-4.315700772	2.222024863	-1.942237841	0.052108314	0.358505201	down_regulated
stress	inter	CvsDi	TRINITY_DN626_c2_g1	332.5820884	4.041887954	2.139696146	1.889000904	0.058891708	0.385014649	up_regulated
stress	inter	CvsDi	TRINITY_DN7376_c0_g1	74.34133983	4.952630085	2.576941081	1.921902725	0.054617999	0.368403758	up_regulated
stress	inter	CvsDi	TRINITY_DN112275_c0_g1	31.56943358	4.995464371	2.369200206	2.108502421	0.034987554	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN15751_c0_g1	6.664906977	6.205212332	3.29192615	1.884979204	0.059432646	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN29121_c0_g1	6.664906977	6.205212332	3.29192615	1.884979204	0.059432646	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN1191980_c0_g1	6.753044897	6.224606205	3.304538209	1.883653876	0.059611809	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN705856_c0_g1	7.343833043	6.345123783	3.253650088	1.950155552	0.051157582	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN912320_c0_g1	21.24970682	6.373225194	3.324088069	1.917285301	0.055201696	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN8287_c3_g1	8.199034952	6.50463967	3.259566582	1.995553552	0.04598254	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN793628_c0_g1	8.240486182	6.511821955	3.241007781	2.009196643	0.044516284	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN20368_c0_g1	8.452977794	6.548552484	3.233888154	2.024978036	0.04286962	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN886150_c0_g1	8.940137864	6.629263865	3.201806957	2.070475814	0.038407809	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN981588_c0_g1	9.028275785	6.643654809	3.246193607	2.046598452	0.040697527	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN11388_c0_g1	26.03114381	6.73410199	2.817300028	2.39026796	0.016836084	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN567360_c0_g1	9.670769857	6.741667412	3.223653289	2.091312808	0.036500032	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN56646_c0_g2	10.4430693	6.853094222	3.139159512	2.183098436	0.029028562	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN914897_c0_g1	10.64530622	6.881359955	3.329167525	2.066991193	0.038734984	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN73955_c0_g1	10.96120969	6.922405751	3.634953535	1.904400066	0.056858101	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN182419_c0_g1	11.26205544	6.962475524	3.623175829	1.921649915	0.054649823	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN131728_c0_g1	11.51599828	6.99474443	3.26098596	2.144978395	0.031954574	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN391824_c0_g1	11.91502043	7.043695967	3.137066607	2.245312851	0.024748056	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN33856_c0_g1	12.12729582	7.068298152	3.613277177	1.956201478	0.050441425	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN94523_c0_g1	12.17921797	7.074753284	3.107226906	2.276870502	0.02279396	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN1337343_c0_g1	13.81195479	7.256840617	3.58330341	2.025181735	0.042848706	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN520918_c0_g1	13.97230803	7.272601067	3.290462044	2.210206643	0.027090824	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN43163_c0_g1	14.1021134	7.286759241	3.094721046	2.35457708	0.018543797	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN121998_c0_g1	14.4594681	7.322120389	3.581389435	2.044491536	0.040905013	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN67061_c0_g2	79.5103538	7.397899737	3.177479408	2.32822901	0.019899947	0.162990038	up_regulated
stress	inter	CvsDi	TRINITY_DN551_c0_g1	18.94251756	7.711908531	3.04242927	2.534786464	0.011251589	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN1089_c0_g1	19.87538647	7.781264635	3.03782116	2.561462385	0.010423253	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN181468_c0_g1	20.75633323	7.843765675	3.535179592	2.218774314	0.026502082	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN2512_c4_g1	20.76178492	7.844526819	2.999716952	2.615089005	0.008920421	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN55029_c0_g1	21.54455528	7.898187326	3.13679688	2.517914812	0.011805187	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN892195_c0_g1	24.7210261	8.095994084	3.520356303	2.299765532	0.021461507	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN14319_c0_g1	79.25937132	22.50075586	3.491405347	6.444612879	1.16E-10	3.32E-09	up_regulated
stress	inter	CvsDi	TRINITY_DN3901_c0_g2	199.8671638	23.77579815	3.524446288	6.745966943	1.52E-11	7.27E-10	up_regulated
stress	inter	CvsDr	TRINITY_DN725520_c0_g1	479.2968606	-25.51038043	3.653535957	-6.982381104	2.90E-12	4.11E-10	down_regulated
stress	inter	CvsDr	TRINITY_DN2160_c0_g1	207.1140898	-24.36699114	3.614579067	-6.741308101	1.57E-11	7.42E-10	down_regulated
stress	inter	CvsDr	TRINITY_DN221_c0_g2	722.5096849	-9.451480431	2.93965068	-3.215171278	0.001303667	0.024595853	down_regulated
stress	inter	CvsDr	TRINITY_DN1351665_c0_g1	66.53291465	-9.374831668	3.577800017	-2.620278278	0.008785804	0.09027918	down_regulated
stress	inter	CvsDr	TRINITY_DN17808_c0_g1	52.70925401	-9.038803838	3.575559855	-2.527940855	0.011473368	0.09027918	down_regulated
stress	inter	CvsDr	TRINITY_DN38017_c0_g1	46.678958	-8.863521729	3.575093871	-2.479241678	0.013166206	0.096583235	down_regulated
stress	inter	CvsDr	TRINITY_DN9736_c0_g1	46.23226941	-8.849649541	3.575090681	-2.475363657	0.013310057	0.096583235	down_regulated
stress	inter	CvsDr	TRINITY_DN18826_c0_g1	36.62846465	-8.513716848	3.576723714	-2.380311573	0.017298005	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN31824_c1_g1	31.38762086	-8.290989271	3.579260641	-2.316397184	0.020536592	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN35861_c0_g2	21.21980002	-7.726223043	3.596889494	-2.148029027	0.03171145	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN44472_c0_g1	18.98426522	-7.565555154	3.605651791	-2.098248969	0.035883158	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN284222_c0_g1	17.02004793	-7.408058667	3.614628686	-2.04946602	0.040416567	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN347341_c0_g1	16.35692918	-7.350727585	3.618556945	-2.031397515	0.042214686	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN684852_c0_g1	15.61566306	-7.283769245	3.171515895	-2.29662076	0.021640413	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN45156_c0_g2	14.51737928	-7.178528106	3.632490068	-1.97620034	0.048132091	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN26986_c0_g1	14.39983891	-7.166909046	3.142518825	-2.280625653	0.022570608	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN20083_c0_g1	14.36757293	-7.163649484	3.633128622	-1.971757741	0.048637269	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN19398_c0_g1	13.92549376	-7.118563309	3.63706729	-1.957226178	0.050320882	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN1346004_c0_g1	13.26237501	-7.048176871	3.643569373	-1.934415445	0.053062069	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN11490_c0_g1	12.50728061	-6.963511397	3.652606408	-1.906449975	0.05659185	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN32288_c0_g1	33.70660478	-6.961156511	2.900116106	-2.400302697	0.016381519	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN184502_c0_g1	9.822539627	-6.61491577	3.326140855	-1.988765978	0.04672704	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN332_c0_g1	155.8337816	4.338049753	2.292941718	1.89191453	0.05850237	0.312380577	up_regulated
stress	inter	CvsDr	TRINITY_DN8060_c0_g1	83.4530869	4.490388121	2.356210653	1.905766836	0.056680463	0.308472522	up_regulated
stress	inter	CvsDr	TRINITY_DN2607_c0_g1	210.6417678	4.508927672	2.320416983	1.943154056	0.051997547	0.293673665	up_regulated
stress	inter	CvsDr	TRINITY_DN14273_c0_g1	1403.317543	4.958341086	2.561729414	1.935544425	0.052923523	0.293673665	up_regulated
stress	inter	CvsDr	TRINITY_DN29832_c0_g1	22.92320736	5.058616281	2.550953092	1.983029911	0.047364089	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN21961_c0_g1	43.19424393	5.56629362	2.635141083	2.112332298	0.034657957	0.217960043	up_regulated
stress	inter	CvsDr	TRINITY_DN12967_c2_g1	34.18866187	5.64586331	2.661634689	2.121201431	0.033904855	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN140194_c0_g1	11.99490669	5.728767845	2.989236577	1.91646519	0.05530591	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN890011_c0_g1	264.4140188	5.862895365	2.971182362	1.973253288	0.048466712	0.285751656	up_regulated
stress	inter	CvsDr	TRINITY_DN7807_c0_g1	117.6742147	6.215297743	2.612404116	2.379148656	0.017352676	0.116923986	up_regulated
stress	inter	CvsDr	TRINITY_DN11388_c0_g1	17.95321033	6.314406136	2.936309799	2.150456378	0.031519133	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN626_c0_g2	180.5643059	6.325165069	2.431337323	2.601516873	0.00928125	0.09027918	up_regulated
stress	inter	CvsDr	TRINITY_DN31886_c0_g3	36.47615285	6.347124307	2.70215281	2.348913904	0.01882826	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN48993_c0_g1	18.49968156	6.359209643	2.950182075	2.155531246	0.031120286	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN35789_c0_g1	6.923524624	6.404810438	3.300465483	1.940577919	0.052309495	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN31886_c0_g2	6.944122109	6.407974794	3.299694975	1.941990046	0.052138305	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN7601_c0_g1	7.110123228	6.443455388	3.3068735	1.948503742	0.051354718	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN15730_c2_g1	7.510580213	6.520342119	3.425919443	1.903238598	0.057009419	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN43163_c0_g1	7.510580213	6.520342119	3.425919443	1.903238598	0.057009419	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN62753_c0_g1	7.64204865	6.546609828	3.259968338	2.008182028	0.044623952	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN33856_c0_g1	7.676581332	6.553003686	3.259698549	2.010309722	0.044398419	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN143_c0_g1	7.773517087	6.572453885	3.328020663	1.974883737	0.048281343	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN12925_c0_g1	9.50742408	6.86071887	3.324794184	2.063501826	0.039064974	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN233774_c0_g1	10.44707939	6.996883684	3.675237011	1.903791147	0.05693739	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN713837_c0_g1	203.2799762	7.013963855	2.980071432	2.353622728	0.01859147	0.122357811	up_regulated
stress	inter	CvsDr	TRINITY_DN228084_c0_g1	10.73727604	7.036432374	3.670142343	1.917209665	0.055211301	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN355739_c0_g1	11.31766934	7.112419183	3.660831619	1.94284248	0.052035193	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN2512_c1_g1	11.56000873	7.144424761	3.18340637	2.244270423	0.024815007	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN279814_c0_g1	11.57333331	7.144536811	3.347514667	2.134280958	0.032819789	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN372841_c0_g1	12.47845594	7.253348054	3.6451434	1.989866312	0.046605664	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN26578_c0_g1	13.12852522	7.327934401	3.159589494	2.319267872	0.020380516	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN880286_c0_g1	13.63924254	7.381728414	3.632516909	2.032124997	0.042141003	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN56381_c0_g1	14.57284618	7.47857363	3.61629799	2.068019187	0.038638219	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN713303_c0_g1	14.57284618	7.47857363	3.61629799	2.068019187	0.038638219	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN36486_c0_g1	14.82789953	7.50258389	3.12597278	2.400079725	0.016391501	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN12967_c0_g2	15.63608641	7.578825557	3.314781206	2.286372791	0.022232459	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN15239_c0_g4	17.12160234	7.70990927	3.606615662	2.137713023	0.032540042	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN14319_c0_g1	17.6408138	7.754101247	3.59606546	2.156273665	0.031062302	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN78892_c0_g1	17.99219229	7.781486726	3.602054719	2.160291093	0.030750142	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN25555_c0_g1	20.96444538	8.003045285	3.582402538	2.23398828	0.025483844	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN78733_c0_g1	24.20385517	8.209422751	3.169332694	2.590268534	0.009590109	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN1089_c0_g1	26.30551832	8.330399504	3.087444039	2.698154006	0.006972518	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN37227_c1_g1	28.16301023	8.42806171	3.132589505	2.690445619	0.007135666	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN1281249_c0_g1	82.29147805	8.523753993	3.36979272	2.529459436	0.011423838	0.09027918	up_regulated
stress	inter	CvsDr	TRINITY_DN2512_c4_g1	31.09345773	8.571322801	3.019539165	2.838619515	0.004530915	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN36029_c0_g1	31.92163148	8.608865409	3.571137846	2.410678551	0.015922875	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN52933_c0_g1	88.3328655	8.627818272	2.93102096	2.943622168	0.003243958	0.043551813	up_regulated
stress	inter	CvsDr	TRINITY_DN33386_c1_g1	33.53861584	8.680154312	3.102507388	2.79778683	0.005145406	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN551_c0_g1	34.18263345	8.708205859	3.059191346	2.846571161	0.004419285	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN506168_c0_g1	36.30428347	8.795042138	3.560919144	2.469879765	0.013515847	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN1161095_c0_g1	40.19437316	8.941949554	3.114512651	2.871058993	0.004090992	0.050336985	up_regulated
stress	inter	CvsDr	TRINITY_DN16314_c0_g1	49.59880981	9.245130452	3.559381301	2.597398162	0.009393297	0.09027918	up_regulated
stress	inter	CvsDr	TRINITY_DN1343017_c0_g1	50.02713718	9.257613663	3.159202801	2.930363844	0.003385653	0.043551813	up_regulated
stress	inter	CvsDr	TRINITY_DN17761_c0_g2	58.32952661	9.478683702	3.568200642	2.656432374	0.007897229	0.089396631	up_regulated
stress	inter	CvsDr	TRINITY_DN14781_c0_g1	75.5305203	9.851834414	3.004577374	3.278941824	0.001041971	0.021062702	up_regulated
stress	inter	CvsDr	TRINITY_DN907409_c0_g1	92.40006787	10.14240402	3.186676654	3.182752791	0.001458821	0.025433562	up_regulated
stress	inter	CvsDr	TRINITY_DN1360_c0_g1	169.5742762	11.01854574	3.26866332	3.370963804	0.000749057	0.016306394	up_regulated
stress	inter	CvsDr	TRINITY_DN1384_c0_g1	575.6377229	12.781719	3.287145289	3.888394906	0.000100909	0.002379779	up_regulated
stress	inter	CvsDr	TRINITY_DN73751_c0_g1	77.48250549	22.60051361	3.573165298	6.325068034	2.53E-10	7.16E-09	up_regulated
stress	inter	CvsDr	TRINITY_DN2745_c0_g1	350.0039723	24.42766168	3.623773951	6.740945216	1.57E-11	7.42E-10	up_regulated
stress	inter	CvsDxD	TRINITY_DN725520_c0_g1	405.0287167	-25.05156713	3.965301982	-6.317694654	2.65E-10	2.54E-08	down_regulated
stress	inter	CvsDxD	TRINITY_DN221_c0_g2	721.6952017	-10.18098221	3.260005733	-3.122995187	0.001790207	0.018349619	down_regulated
stress	inter	CvsDxD	TRINITY_DN1351665_c0_g1	66.51423874	-9.15243962	3.904374341	-2.34415013	0.019070493	0.088277929	down_regulated
stress	inter	CvsDxD	TRINITY_DN34378_c0_g1	44.63746254	-8.576989655	3.897809078	-2.200464282	0.027773971	0.116377466	down_regulated
stress	inter	CvsDxD	TRINITY_DN38017_c0_g1	39.44594678	-8.39896981	3.88831681	-2.160052851	0.030768578	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN55012_c0_g1	34.5387955	-8.207341337	3.888367635	-2.110742118	0.034794484	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN31824_c1_g1	31.3788103	-8.068466543	3.897865772	-2.069970342	0.038455122	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN2160_c0_g1	208.2918834	-7.384865249	3.555952829	-2.076761308	0.037823595	0.150769052	down_regulated
stress	inter	CvsDxD	TRINITY_DN26986_c0_g1	13.91216663	-6.894867347	3.414138156	-2.019504494	0.043434811	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN1174_c0_g2	48.97730196	-5.643469991	2.675028989	-2.109685545	0.03488545	0.141015835	down_regulated
stress	inter	CvsDxD	TRINITY_DN44292_c0_g2	48.45882404	5.761297339	3.053411457	1.886839497	0.059181917	0.21500266	up_regulated
stress	inter	CvsDxD	TRINITY_DN7195_c0_g1	167.4208606	6.066916093	3.046794282	1.991245726	0.046453879	0.175424517	up_regulated
stress	inter	CvsDxD	TRINITY_DN33171_c0_g1	223.5344663	6.491032922	2.909778926	2.230764978	0.025696701	0.111273576	up_regulated
stress	inter	CvsDxD	TRINITY_DN52933_c0_g1	19.51177419	6.591416917	3.127544298	2.107537509	0.035071014	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1343017_c0_g1	9.762168962	7.021132393	3.548157951	1.978810552	0.04783734	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN33386_c1_g1	9.965575887	7.051099616	3.451850653	2.042701242	0.041082022	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN66972_c0_g1	9.99302314	7.055623701	3.4697987	2.033438914	0.042008199	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN907409_c0_g1	10.54060473	7.132681689	3.535634842	2.017369442	0.043656974	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN511599_c0_g1	10.72430908	7.156795168	3.508346342	2.039934052	0.041356895	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN5404_c0_g1	83.51719869	7.181667016	3.502434763	2.050478453	0.040317764	0.154282645	up_regulated
stress	inter	CvsDxD	TRINITY_DN43163_c0_g1	13.29400204	7.467134208	3.388387603	2.20374263	0.027542449	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN291433_c0_g1	13.38972656	7.477105216	3.911551256	1.911544737	0.055934613	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN363_c2_g1	13.38972656	7.477105216	3.911551256	1.911544737	0.055934613	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN715182_c0_g1	13.71630525	7.511876942	3.90925762	1.92156099	0.054661021	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN390661_c0_g1	14.04288395	7.545830329	3.907099738	1.93131244	0.053444426	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN896959_c0_g1	14.04288395	7.545830329	3.907099738	1.93131244	0.053444426	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN2512_c4_g1	14.31878135	7.574489488	3.433600877	2.205990085	0.027384694	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN384052_c0_g1	14.36946265	7.579003081	3.905067794	1.940812165	0.052281065	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN588089_c0_g1	14.36946265	7.579003081	3.905067794	1.940812165	0.052281065	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1331259_c0_g1	14.52218828	7.594809939	3.900944631	1.946915595	0.051544855	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN11388_c0_g1	39.17334838	7.60476302	3.252936264	2.337814947	0.01939685	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN65477_c0_g1	14.69604134	7.61142984	3.903152865	1.950072186	0.051167516	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN14781_c0_g1	15.1028552	7.650970878	3.385551733	2.259888929	0.023828145	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN33451_c0_g1	16.00235613	7.734306381	3.89651079	1.984931339	0.047152111	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN741768_c0_g1	16.32893483	7.763457198	3.895072018	1.993148564	0.046245182	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN890758_c0_g1	16.98209222	7.820049255	3.892422051	2.009044536	0.044532411	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1346363_c0_g1	17.30867092	7.847534227	3.891201558	2.016738046	0.043722857	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN61338_c0_g1	17.61201557	7.873061508	3.886920616	2.025526705	0.042813309	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN75126_c0_g1	17.96182831	7.900980588	3.888948925	2.031649359	0.042189166	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN56597_c0_g1	18.22998103	7.922806444	3.884888595	2.039390899	0.04141103	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN809130_c0_g1	18.28840701	7.926979348	3.887909443	2.038879626	0.041462043	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN922286_c0_g1	19.15692921	7.994348427	3.882197129	2.059232996	0.039471924	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN39930_c0_g1	19.2681431	8.002277419	3.885099705	2.059735406	0.039423843	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN551244_c0_g1	19.59472179	8.02652808	3.884256595	2.06642581	0.038788291	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN193763_c0_g1	20.70184286	8.10622458	3.87850665	2.090037561	0.036614426	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN508266_c0_g1	20.70184286	8.10622458	3.87850665	2.090037561	0.036614426	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1128682_c0_g1	22.86050876	8.248946442	3.877830892	2.127206336	0.033402943	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1297832_c0_g1	22.86050876	8.248946442	3.877830892	2.127206336	0.033402943	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN56151_c0_g1	24.16682354	8.329125078	3.876051388	2.148868589	0.031644818	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN568874_c0_g1	25.02760107	8.379948539	3.871884679	2.164307368	0.030440768	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1149599_c0_g1	27.75918921	8.529081384	3.872718572	2.202349906	0.027640601	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN21213_c0_g1	28.7389253	8.57912632	3.872113192	2.21561868	0.026717628	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN897902_c0_g1	30.02764412	8.642412811	3.541438224	2.440368083	0.014672304	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN62753_c0_g1	31.0073802	8.688737663	3.541432351	2.453452954	0.014149205	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN20105_c0_g2	32.00471226	8.734416698	3.870765391	2.256508937	0.024038776	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN213_c0_g2	581.7845229	8.771971781	3.127403536	2.804873654	0.005033631	0.042489765	up_regulated
stress	inter	CvsDxD	TRINITY_DN53446_c0_g1	34.29076314	8.83395979	3.870294781	2.282503088	0.022459655	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN350683_c0_g1	36.15097932	8.910405889	3.866979304	2.304229009	0.021209792	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN101850_c0_g1	36.25023532	8.914134978	3.87012225	2.303321291	0.021260773	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN755426_c0_g1	42.94859937	9.158960534	3.867497384	2.368187907	0.017875455	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN541786_c0_g1	44.74128143	9.217768017	3.870961204	2.38126076	0.017253494	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN52786_c0_g1	45.06786012	9.228260895	3.871028621	2.383929905	0.017128865	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN905987_c0_g1	46.37417491	9.269485568	3.871316296	2.394401506	0.016647514	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN928375_c0_g1	48.81927122	9.343785804	3.868806205	2.415159951	0.015728301	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN494488_c0_g1	64.57739041	9.747337054	3.873906891	2.516151608	0.011864413	0.078535939	up_regulated
stress	inter	CvsDxD	TRINITY_DN26077_c0_g1	69.56126236	9.854468201	3.878775714	2.540613051	0.011065831	0.075616514	up_regulated
stress	inter	CvsDxD	TRINITY_DN107416_c0_g2	70.21441976	9.867951761	3.879011306	2.543934777	0.010961155	0.075616514	up_regulated
stress	inter	CvsDxD	TRINITY_DN8217_c0_g1	70.21441976	9.867951761	3.879011306	2.543934777	0.010961155	0.075616514	up_regulated
stress	inter	CvsDxD	TRINITY_DN2872_c1_g1	72.17389194	9.907662673	3.87971932	2.553706043	0.01065832	0.075616514	up_regulated
stress	inter	CvsDxD	TRINITY_DN6859_c0_g1	102.8546934	10.41874909	3.570667113	2.917871858	0.003524291	0.034878333	up_regulated
stress	inter	CvsDxD	TRINITY_DN5243_c0_g1	179.8744779	11.22514724	3.501193977	3.206091211	0.001345513	0.014567711	up_regulated
stress	inter	CvsDxD	TRINITY_DN6859_c0_g2	76.74599369	22.70623482	3.881372986	5.850052263	4.91E-09	5.64E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN29373_c0_g1	83.73431963	22.76507247	3.880828123	5.866034709	4.46E-09	5.57E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN11181_c0_g1	90.13572025	22.92393512	3.886142983	5.898891322	3.66E-09	5.35E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN13658_c0_g1	123.284109	22.9838824	3.893932844	5.902485564	3.58E-09	5.35E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN68917_c1_g1	105.1583403	23.14421386	3.89123765	5.947777017	2.72E-09	5.20E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN6710_c0_g1	117.5683308	23.29587586	3.895196658	5.980667449	2.22E-09	5.20E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN19981_c0_g1	187.2435339	23.94576319	3.910635959	6.123240168	9.17E-10	2.92E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN5378_c0_g1	432.575821	24.33103179	3.948333149	6.16235532	7.17E-10	2.85E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN4654_c0_g1	490.3555913	25.29069942	3.954234534	6.395852143	1.60E-10	2.29E-08	up_regulated
stress	under	CvsDi	TRINITY_DN12272_c0_g1	128.2801401	-24.10771384	3.246471037	-7.425821319	1.12E-13	1.21E-11	down_regulated
stress	under	CvsDi	TRINITY_DN27953_c0_g1	97.13393322	-23.72181938	3.23755358	-7.327081635	2.35E-13	2.29E-11	down_regulated
stress	under	CvsDi	TRINITY_DN14706_c0_g1	99.77344228	-23.62409023	3.238318852	-7.29517114	2.98E-13	2.63E-11	down_regulated
stress	under	CvsDi	TRINITY_DN748627_c0_g1	101.0931968	-23.47815193	3.238701939	-7.249247498	4.19E-13	3.14E-11	down_regulated
stress	under	CvsDi	TRINITY_DN813_c0_g1	700.0941208	-13.18839661	2.651103637	-4.974681648	6.54E-07	3.97E-05	down_regulated
stress	under	CvsDi	TRINITY_DN1025768_c0_g1	64.18200735	-9.740692077	3.244578718	-3.00214386	0.002680855	0.068643986	down_regulated
stress	under	CvsDi	TRINITY_DN39912_c0_g1	43.81585031	-9.19099947	3.228512281	-2.846821901	0.004415806	0.089020367	down_regulated
stress	under	CvsDi	TRINITY_DN1405_c2_g1	37.72020771	-8.974156794	2.713634402	-3.307061845	0.000942801	0.036166987	down_regulated
stress	under	CvsDi	TRINITY_DN997627_c0_g1	30.61830504	-8.674201906	3.237568598	-2.679233395	0.007379094	0.112025547	down_regulated
stress	under	CvsDi	TRINITY_DN274_c0_g1	30.64546664	-8.673724662	2.801810655	-3.095756898	0.001963112	0.061616398	down_regulated
stress	under	CvsDi	TRINITY_DN100125_c0_g1	29.29855051	-8.61067458	3.239497572	-2.658027793	0.007859941	0.114145115	down_regulated
stress	under	CvsDi	TRINITY_DN727217_c0_g1	28.50669779	-8.571170756	3.240800435	-2.644769688	0.008174654	0.116969675	down_regulated
stress	under	CvsDi	TRINITY_DN24985_c0_g1	23.08665527	-8.264667446	2.934078974	-2.816784251	0.00485071	0.092543934	down_regulated
stress	under	CvsDi	TRINITY_DN89607_c0_g1	22.08421303	-8.200796258	2.832317042	-2.895437247	0.003786307	0.084203407	down_regulated
stress	under	CvsDi	TRINITY_DN1245700_c0_g1	21.47150421	-8.15994316	2.969255698	-2.748144313	0.005993362	0.103461472	down_regulated
stress	under	CvsDi	TRINITY_DN206411_c0_g1	19.90161267	-8.053336963	2.885697648	-2.790776424	0.005258178	0.094615278	down_regulated
stress	under	CvsDi	TRINITY_DN1760029_c0_g1	18.47656338	-7.946034937	3.273884916	-2.427096596	0.015220203	0.151114871	down_regulated
stress	under	CvsDi	TRINITY_DN14038_c0_g1	17.94866157	-7.904252226	3.27707179	-2.411986289	0.015865878	0.155934338	down_regulated
stress	under	CvsDi	TRINITY_DN44060_c0_g1	17.22922998	-7.843802567	2.806495802	-2.794874149	0.005191992	0.094615278	down_regulated
stress	under	CvsDi	TRINITY_DN606547_c0_g1	15.30915252	-7.67499075	3.297182792	-2.327741965	0.019925809	NA	down_regulated
stress	under	CvsDi	TRINITY_DN459349_c0_g1	14.78125071	-7.624416828	3.302258513	-2.30884917	0.020951952	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1381219_c0_g1	14.52540166	-7.595836254	3.321153168	-2.287108083	0.022189515	NA	down_regulated
stress	under	CvsDi	TRINITY_DN372122_c0_g1	14.18760163	-7.561853188	3.324779266	-2.274392549	0.022942395	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1647317_c0_g1	13.98939799	-7.54506614	3.31072361	-2.27897796	0.022668375	NA	down_regulated
stress	under	CvsDi	TRINITY_DN490_c1_g1	36.25466052	-7.472933418	2.635434025	-2.835560802	0.004574531	0.089020367	down_regulated
stress	under	CvsDi	TRINITY_DN54089_c0_g1	12.78446809	-7.412777874	2.869496207	-2.583302901	0.009785936	NA	down_regulated
stress	under	CvsDi	TRINITY_DN105714_c0_g1	12.66964346	-7.40226905	3.327588506	-2.224514551	0.026113845	NA	down_regulated
stress	under	CvsDi	TRINITY_DN61969_c0_g1	12.57387857	-7.390268102	2.877792956	-2.568033286	0.010227733	NA	down_regulated
stress	under	CvsDi	TRINITY_DN189467_c0_g1	12.49860143	-7.378787479	3.346345298	-2.205028717	0.027452079	NA	down_regulated
stress	under	CvsDi	TRINITY_DN112631_c0_g1	12.14174165	-7.340940635	3.335513166	-2.20084295	0.027747144	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1266_c0_g1	100.2001264	-7.324709072	2.695209748	-2.717676826	0.006574202	0.107184558	down_regulated
stress	under	CvsDi	TRINITY_DN297102_c0_g1	11.87779075	-7.309269803	3.339773613	-2.188552474	0.028629383	NA	down_regulated
stress	under	CvsDi	TRINITY_DN40893_c0_g1	11.42231006	-7.249236037	2.922059837	-2.480865021	0.013106399	NA	down_regulated
stress	under	CvsDi	TRINITY_DN82726_c0_g1	11.34988894	-7.2437613	3.348960708	-2.162987844	0.030542115	NA	down_regulated
stress	under	CvsDi	TRINITY_DN78407_c0_g1	11.08593803	-7.209856737	3.353919263	-2.149681066	0.031580451	NA	down_regulated
stress	under	CvsDi	TRINITY_DN65613_c0_g1	10.55803622	-7.139557724	3.364657906	-2.121926782	0.033843887	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1561079_c0_g1	10.44178358	-7.123911261	3.036962472	-2.345735691	0.018989568	NA	down_regulated
stress	under	CvsDi	TRINITY_DN2871_c0_g4	10.40890995	-7.115219956	2.940609727	-2.419641032	0.015535835	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1584271_c0_g1	9.766183503	-7.027232526	3.38315219	-2.077125748	0.037789955	NA	down_regulated
stress	under	CvsDi	TRINITY_DN24985_c5_g1	9.722351989	-7.015560292	3.123076807	-2.246361753	0.024680846	NA	down_regulated
stress	under	CvsDi	TRINITY_DN105581_c0_g1	9.238281692	-6.947172039	3.397388729	-2.044856386	0.040869019	NA	down_regulated
stress	under	CvsDi	TRINITY_DN37724_c0_g1	23.84611195	-6.86312022	3.098992426	-2.214629556	0.026785502	0.19395664	down_regulated
stress	under	CvsDi	TRINITY_DN450160_c0_g1	8.710379881	-6.862404758	3.413470655	-2.010389264	0.044390006	NA	down_regulated
stress	under	CvsDi	TRINITY_DN693651_c0_g1	8.446428976	-6.818075505	3.422310011	-1.992243684	0.046344327	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1574_c0_g1	23.1480397	-6.804049427	2.633581156	-2.583573099	0.009778274	0.128571094	down_regulated
stress	under	CvsDi	TRINITY_DN413820_c0_g1	8.350664086	-6.798752964	2.982549458	-2.279510553	0.022636733	NA	down_regulated
stress	under	CvsDi	TRINITY_DN575_c0_g1	385.7771751	-6.792848828	1.98149024	-3.428151546	0.000607706	0.027878898	down_regulated
stress	under	CvsDi	TRINITY_DN1068_c0_g1	8.075755301	-6.748105726	3.044769321	-2.216294575	0.026671334	NA	down_regulated
stress	under	CvsDi	TRINITY_DN331873_c0_g1	8.055267553	-6.747957222	2.999981554	-2.249332904	0.024491324	NA	down_regulated
stress	under	CvsDi	TRINITY_DN295_c0_g1	42.8871898	-6.702160757	2.327748142	-2.879246528	0.003986266	0.084203407	down_regulated
stress	under	CvsDi	TRINITY_DN477371_c0_g1	7.654576259	-6.676270586	3.452667708	-1.933655698	0.053155474	NA	down_regulated
stress	under	CvsDi	TRINITY_DN41433_c0_g1	7.357751718	-6.612889135	3.191232181	-2.072205581	0.038246273	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1030866_c0_g1	7.231969209	-6.593656876	3.062130111	-2.153290891	0.031295824	NA	down_regulated
stress	under	CvsDi	TRINITY_DN24472_c0_g1	55.33250204	-6.505538239	2.466092544	-2.637994366	0.008339797	0.117603221	down_regulated
stress	under	CvsDi	TRINITY_DN79947_c0_g1	6.513965626	-6.442297783	3.087214186	-2.086767356	0.036909169	NA	down_regulated
stress	under	CvsDi	TRINITY_DN33906_c0_g1	32.88127076	-6.295143891	2.929723077	-2.148716355	0.031656891	0.209538472	down_regulated
stress	under	CvsDi	TRINITY_DN185381_c0_g1	14.88416705	-6.174468172	2.765588882	-2.232605219	0.02557499	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1882_c1_g1	5.236614604	-6.12878903	3.211942068	-1.908125645	0.056374979	NA	down_regulated
stress	under	CvsDi	TRINITY_DN111151_c0_g1	5.18325322	-6.107398344	3.230726355	-1.890410289	0.058703108	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1574628_c0_g1	5.18325322	-6.107398344	3.230726355	-1.890410289	0.058703108	NA	down_regulated
stress	under	CvsDi	TRINITY_DN8661_c0_g1	13.76205975	-6.041708523	2.813481862	-2.147413354	0.031760388	NA	down_regulated
stress	under	CvsDi	TRINITY_DN132495_c0_g1	12.90874378	-5.95265181	2.803629477	-2.123194901	0.033737523	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1616189_c0_g1	24.69879269	-5.876453132	2.940766246	-1.998272777	0.045687095	0.275267188	down_regulated
stress	under	CvsDi	TRINITY_DN301_c0_g2	108.517521	-5.875058135	2.041148469	-2.878310041	0.00399812	0.084203407	down_regulated
stress	under	CvsDi	TRINITY_DN28255_c0_g1	11.28120684	-5.748751447	2.942267792	-1.953850517	0.050718898	NA	down_regulated
stress	under	CvsDi	TRINITY_DN9135_c0_g1	11.00772606	-5.717181059	2.755301992	-2.074974386	0.037988909	NA	down_regulated
stress	under	CvsDi	TRINITY_DN178514_c0_g1	10.91196117	-5.701025774	2.814453863	-2.025624171	0.042803313	NA	down_regulated
stress	under	CvsDi	TRINITY_DN131121_c0_g1	39.98739005	-5.573858099	2.263948533	-2.462007426	0.013816181	0.146121128	down_regulated
stress	under	CvsDi	TRINITY_DN4855_c0_g1	36.29778941	-5.433128349	2.326974218	-2.334846818	0.019551424	0.164412176	down_regulated
stress	under	CvsDi	TRINITY_DN10044_c0_g2	8.857127737	-5.415207794	2.813227099	-1.92490958	0.054240671	NA	down_regulated
stress	under	CvsDi	TRINITY_DN2871_c0_g2	26.38398645	-5.38882184	2.277514942	-2.366097249	0.017976722	0.162153655	down_regulated
stress	under	CvsDi	TRINITY_DN24837_c0_g1	119.5213666	-5.263122759	2.002875498	-2.627783287	0.008594325	0.117778564	down_regulated
stress	under	CvsDi	TRINITY_DN9798_c0_g1	31.56232042	-5.213745791	2.37390675	-2.196272365	0.028072455	0.197490173	down_regulated
stress	under	CvsDi	TRINITY_DN384079_c0_g1	81.98982961	-5.131301123	2.661995082	-1.927614802	0.053903056	0.301423413	down_regulated
stress	under	CvsDi	TRINITY_DN2123_c2_g2	258.9223264	-5.053241171	1.994936856	-2.533033142	0.011308026	0.132698631	down_regulated
stress	under	CvsDi	TRINITY_DN48378_c0_g1	14.05452145	-5.043064006	2.57405106	-1.959193462	0.050090135	NA	down_regulated
stress	under	CvsDi	TRINITY_DN173378_c0_g1	46.73315833	-4.970223846	2.22739549	-2.231406083	0.025654242	0.187681036	down_regulated
stress	under	CvsDi	TRINITY_DN24323_c0_g1	38.24956028	-4.895728568	2.361841306	-2.072843995	0.038186801	0.238177929	down_regulated
stress	under	CvsDi	TRINITY_DN2108_c0_g1	257.1693286	-4.806378625	2.307106784	-2.083292658	0.037224556	0.23519151	down_regulated
stress	under	CvsDi	TRINITY_DN21648_c0_g1	16.56388516	-4.682098169	2.479253587	-1.888511201	0.058957356	0.320477693	down_regulated
stress	under	CvsDi	TRINITY_DN18_c4_g1	70.97655871	-4.576462078	2.028585178	-2.255987142	0.024071436	0.180165443	down_regulated
stress	under	CvsDi	TRINITY_DN21271_c0_g1	35.31982173	-4.544297063	2.378522772	-1.910554364	0.056061874	0.308181939	down_regulated
stress	under	CvsDi	TRINITY_DN11515_c1_g1	33.391726	-4.096133508	2.084225236	-1.965302712	0.049379212	0.287700438	down_regulated
stress	under	CvsDi	TRINITY_DN490_c0_g1	215.8448136	-3.879612539	1.942769878	-1.99694909	0.045830714	0.275267188	down_regulated
stress	under	CvsDr	TRINITY_DN748627_c0_g1	115.0791413	-24.09823754	3.505493006	-6.874421801	6.22E-12	1.21E-09	down_regulated
stress	under	CvsDr	TRINITY_DN14706_c0_g1	113.5768026	-24.0701056	3.505015312	-6.86733251	6.54E-12	1.21E-09	down_regulated
stress	under	CvsDr	TRINITY_DN12447_c0_g1	81.85167538	-10.19443175	2.946707445	-3.459600907	0.000540977	0.020984195	down_regulated
stress	under	CvsDr	TRINITY_DN97141_c0_g1	79.05008129	-10.1441951	3.223224548	-3.147219485	0.001648312	0.036812297	down_regulated
stress	under	CvsDr	TRINITY_DN4850_c0_g1	73.90417525	-10.04708701	2.950181731	-3.40558241	0.000660231	0.021156092	down_regulated
stress	under	CvsDr	TRINITY_DN1936_c0_g1	59.83904919	-9.74254555	2.958861222	-3.292667286	0.000992418	0.029256494	down_regulated
stress	under	CvsDr	TRINITY_DN26363_c0_g1	47.19505833	-9.400142989	2.981370927	-3.152959903	0.00161624	0.036812297	down_regulated
stress	under	CvsDr	TRINITY_DN1774784_c0_g1	41.35884828	-9.209382649	2.962757706	-3.108381974	0.001881148	0.038511273	down_regulated
stress	under	CvsDr	TRINITY_DN69544_c0_g1	37.5855343	-9.071758355	3.206669024	-2.829028593	0.004668953	0.074804743	down_regulated
stress	under	CvsDr	TRINITY_DN264540_c0_g1	37.01166666	-9.049575476	3.153901892	-2.86932688	0.004113464	0.072181507	down_regulated
stress	under	CvsDr	TRINITY_DN33906_c0_g1	36.95753101	-9.047386838	3.483724576	-2.597044238	0.009402981	0.099501893	down_regulated
stress	under	CvsDr	TRINITY_DN5147_c0_g3	35.09786728	-8.972986959	3.043396922	-2.94834594	0.003194793	0.058932249	down_regulated
stress	under	CvsDr	TRINITY_DN997627_c0_g1	34.85425689	-8.962867825	3.484231372	-2.572408909	0.010099353	0.099501893	down_regulated
stress	under	CvsDr	TRINITY_DN683076_c0_g1	28.57150234	-8.676242568	3.207334506	-2.705125565	0.006827858	0.086631267	down_regulated
stress	under	CvsDr	TRINITY_DN1616189_c0_g1	27.64303133	-8.628513449	3.488707766	-2.473269195	0.013388325	0.113624576	down_regulated
stress	under	CvsDr	TRINITY_DN95445_c0_g1	23.37690405	-8.386650903	2.997829827	-2.79757404	0.005148796	NA	down_regulated
stress	under	CvsDr	TRINITY_DN206411_c0_g1	22.36995126	-8.323316412	3.091097368	-2.692673643	0.007088161	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1760029_c0_g1	21.03274123	-8.234328456	3.500117865	-2.352586048	0.018643376	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1245700_c0_g1	20.90813977	-8.224943171	3.167045228	-2.59704001	0.009403097	NA	down_regulated
stress	under	CvsDr	TRINITY_DN14038_c0_g1	20.43180576	-8.192519529	3.501790448	-2.339523067	0.019308379	NA	down_regulated
stress	under	CvsDr	TRINITY_DN50635_c0_g1	18.6533455	-8.061132922	3.039059747	-2.652508866	0.007989603	NA	down_regulated
stress	under	CvsDr	TRINITY_DN84704_c0_g1	18.40973511	-8.042316692	3.136339245	-2.564236858	0.010340295	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1248128_c0_g1	17.47854239	-7.967265629	3.038796631	-2.621848908	0.008745419	NA	down_regulated
stress	under	CvsDr	TRINITY_DN606547_c0_g1	17.42712845	-7.963104272	3.512797188	-2.266884151	0.023397304	NA	down_regulated
stress	under	CvsDr	TRINITY_DN670357_c0_g1	17.20514248	-7.94449698	3.035930484	-2.616824404	0.008875198	NA	down_regulated
stress	under	CvsDr	TRINITY_DN459349_c0_g1	16.82619298	-7.912493956	3.515672545	-2.25063451	0.024408696	NA	down_regulated
stress	under	CvsDr	TRINITY_DN834672_c0_g1	16.82619298	-7.912493956	3.515672545	-2.25063451	0.024408696	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1112731_c0_g1	16.27939316	-7.864959901	3.174020541	-2.47791714	0.013215183	NA	down_regulated
stress	under	CvsDr	TRINITY_DN105714_c0_g2	15.92478979	-7.833085335	3.520536293	-2.224969347	0.026083296	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1647317_c0_g1	15.92478979	-7.833085335	3.520536293	-2.224969347	0.026083296	NA	down_regulated
stress	under	CvsDr	TRINITY_DN105714_c0_g1	14.42245113	-7.690176626	3.530439675	-2.178248982	0.029387505	NA	down_regulated
stress	under	CvsDr	TRINITY_DN132495_c0_g1	14.23025468	-7.670907105	3.133134154	-2.448317477	0.014352513	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1381219_c0_g1	14.0840288	-7.654829685	3.538470428	-2.163315998	0.030516883	NA	down_regulated
stress	under	CvsDr	TRINITY_DN112631_c0_g1	13.82151566	-7.628798306	3.535175614	-2.157968695	0.030930266	NA	down_regulated
stress	under	CvsDr	TRINITY_DN297102_c0_g1	13.52104793	-7.59710129	3.537740719	-2.147444342	0.031757924	NA	down_regulated
stress	under	CvsDr	TRINITY_DN710736_c0_g1	13.27199415	-7.57003562	3.065875315	-2.469127033	0.013544313	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1698353_c0_g1	13.10414383	-7.55099976	3.075536751	-2.455181118	0.014081362	NA	down_regulated
stress	under	CvsDr	TRINITY_DN152598_c0_g2	13.05545158	-7.546603888	3.12582263	-2.414277706	0.01576644	NA	down_regulated
stress	under	CvsDr	TRINITY_DN82726_c0_g1	12.92011247	-7.531538017	3.543313303	-2.125563667	0.033539608	NA	down_regulated
stress	under	CvsDr	TRINITY_DN189467_c0_g1	12.11881548	-7.437939838	3.557225588	-2.090938473	0.03653358	NA	down_regulated
stress	under	CvsDr	TRINITY_DN89078_c0_g1	11.71824154	-7.390731985	3.55656	-2.07805632	0.037704172	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1584271_c0_g1	11.11730608	-7.314815493	3.564470191	-2.052146631	0.040155414	NA	down_regulated
stress	under	CvsDr	TRINITY_DN105581_c0_g1	10.51637061	-7.234680547	3.573440658	-2.024569942	0.042911543	NA	down_regulated
stress	under	CvsDr	TRINITY_DN727217_c0_g1	32.69370562	-7.222845517	3.261224504	-2.214764886	0.026776207	0.163947225	down_regulated
stress	under	CvsDr	TRINITY_DN664173_c0_g1	10.21045949	-7.190977839	3.120643467	-2.304325347	0.021204387	NA	down_regulated
stress	under	CvsDr	TRINITY_DN450160_c0_g1	9.91543515	-7.149830599	3.583669027	-1.995114656	0.046030376	NA	down_regulated
stress	under	CvsDr	TRINITY_DN693651_c0_g1	9.614967418	-7.105457238	3.589330097	-1.979605399	0.047747886	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1266_c0_g1	97.32549219	-7.093943597	2.907413067	-2.439950373	0.01468928	0.113957886	down_regulated
stress	under	CvsDr	TRINITY_DN24985_c5_g1	9.471463227	-7.082093998	3.292532857	-2.150956211	0.031479656	NA	down_regulated
stress	under	CvsDr	TRINITY_DN158570_c0_g1	9.422770973	-7.076341315	3.184008268	-2.222463235	0.026252016	NA	down_regulated
stress	under	CvsDr	TRINITY_DN825658_c0_g1	9.041099776	-7.016858626	3.294475402	-2.129886483	0.033180986	NA	down_regulated
stress	under	CvsDr	TRINITY_DN413820_c0_g1	8.764978171	-6.971212184	3.140911005	-2.219487331	0.026453588	NA	down_regulated
stress	under	CvsDr	TRINITY_DN477371_c0_g1	8.713564223	-6.963508527	3.608960113	-1.929505539	0.053668133	NA	down_regulated
stress	under	CvsDr	TRINITY_DN491096_c0_g1	8.713564223	-6.963508527	3.608960113	-1.929505539	0.053668133	NA	down_regulated
stress	under	CvsDr	TRINITY_DN510588_c0_g1	8.710842528	-6.96244474	3.144948611	-2.213850081	0.026839094	NA	down_regulated
stress	under	CvsDr	TRINITY_DN331873_c0_g1	8.656706885	-6.953623281	3.152919812	-2.205455164	0.027422171	NA	down_regulated
stress	under	CvsDr	TRINITY_DN954588_c0_g1	8.440164312	-6.917684132	3.306532695	-2.092126336	0.036427216	NA	down_regulated
stress	under	CvsDr	TRINITY_DN664299_c0_g1	8.407653102	-6.910295647	3.209718103	-2.152929144	0.031324248	NA	down_regulated
stress	under	CvsDr	TRINITY_DN40449_c2_g1	8.082839243	-6.854528253	3.165005612	-2.165723886	0.030332293	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1363751_c0_g1	7.947500135	-6.830655729	3.201694432	-2.133450232	0.03288781	NA	down_regulated
stress	under	CvsDr	TRINITY_DN109714_c0_g1	7.920432314	-6.825827124	3.216436283	-2.122170789	0.033823399	NA	down_regulated
stress	under	CvsDr	TRINITY_DN55213_c0_g1	51.93303439	-6.785459168	2.771627526	-2.448185806	0.014357759	0.113781382	down_regulated
stress	under	CvsDr	TRINITY_DN218074_c0_g1	7.619964582	-6.770042685	3.224712712	-2.099425062	0.035779447	NA	down_regulated
stress	under	CvsDr	TRINITY_DN314232_c0_g1	6.964893475	-6.64059383	3.293410481	-2.016327411	0.04376575	NA	down_regulated
stress	under	CvsDr	TRINITY_DN118390_c0_g1	6.799764851	-6.605377144	3.221699194	-2.05027743	0.040337366	NA	down_regulated
stress	under	CvsDr	TRINITY_DN476509_c0_g1	6.718561386	-6.588435871	3.254596477	-2.024348001	0.042934358	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1800759_c0_g1	6.637357922	-6.571217731	3.357187862	-1.957357765	0.05030542	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1327091_c0_g1	6.580500584	-6.557633411	3.222938517	-2.034675305	0.041883555	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1276013_c0_g1	6.391025833	-6.516471712	3.286056689	-1.983067344	0.047359908	NA	down_regulated
stress	under	CvsDr	TRINITY_DN76832_c0_g1	6.361236317	-6.508229402	3.243911976	-2.006290384	0.044825275	NA	down_regulated
stress	under	CvsDr	TRINITY_DN91406_c2_g1	16.15425357	-6.477993919	2.94937858	-2.196392814	0.02806384	NA	down_regulated
stress	under	CvsDr	TRINITY_DN67600_c0_g2	6.198829387	-6.471822699	3.248643388	-1.992161627	0.046353327	NA	down_regulated
stress	under	CvsDr	TRINITY_DN384079_c0_g1	91.77907726	-6.423659356	3.116998724	-2.060847605	0.039317581	0.213066598	down_regulated
stress	under	CvsDr	TRINITY_DN1882_c1_g1	5.790090369	-6.374040854	3.313855984	-1.923451376	0.054423387	NA	down_regulated
stress	under	CvsDr	TRINITY_DN27953_c0_g1	112.0312688	-6.263180118	3.155024019	-1.985144988	0.047128343	0.236241366	down_regulated
stress	under	CvsDr	TRINITY_DN372122_c0_g1	13.90475356	-6.262844513	3.305235173	-1.894825689	0.058115499	NA	down_regulated
stress	under	CvsDr	TRINITY_DN156869_c0_g1	5.324494014	-6.252101317	3.298541928	-1.895413626	0.058037625	NA	down_regulated
stress	under	CvsDr	TRINITY_DN358843_c0_g1	12.65146869	-6.119555493	2.889913868	-2.117556361	0.034212657	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1715_c1_g1	299.4665021	-5.797311398	2.917614049	-1.98700421	0.04692193	0.236241366	down_regulated
stress	under	CvsDr	TRINITY_DN34459_c0_g1	32.3234628	-5.680090177	2.585681777	-2.196747576	0.028038479	0.166648056	down_regulated
stress	under	CvsDr	TRINITY_DN14408_c0_g1	21.30150679	-5.476613483	2.670784202	-2.05056383	0.040309442	NA	down_regulated
stress	under	CvsDr	TRINITY_DN21348_c0_g1	156.7664384	-5.381123481	2.406941691	-2.235668401	0.0253735	0.163733396	down_regulated
stress	under	CvsDr	TRINITY_DN27218_c0_g1	18.45214436	-5.285003434	2.697278633	-1.959383569	0.050067883	NA	down_regulated
stress	under	CvsDr	TRINITY_DN24472_c0_g1	63.32256526	-5.200671073	2.701460419	-1.925133175	0.0542127	0.256120254	down_regulated
stress	under	CvsDr	TRINITY_DN156959_c0_g1	22.60022409	-5.153151897	2.613817915	-1.971503779	0.048666281	NA	down_regulated
stress	under	CvsDr	TRINITY_DN42577_c0_g1	34.54122315	-5.148426505	2.711084249	-1.899028593	0.057560717	0.263545208	down_regulated
stress	under	CvsDr	TRINITY_DN125416_c0_g1	48.81120287	-4.942900415	2.278170947	-2.169679331	0.030031147	0.175649053	down_regulated
stress	under	CvsDr	TRINITY_DN2123_c2_g2	264.8737157	-4.247453413	2.213568571	-1.918826219	0.05500633	0.25821443	down_regulated
stress	under	CvsDxD	TRINITY_DN1715_c1_g1	246.5965537	-24.9601343	3.435578857	-7.265190332	3.73E-13	7.07E-11	down_regulated
stress	under	CvsDxD	TRINITY_DN12272_c0_g1	122.4166753	-24.01799464	3.404872542	-7.05400697	1.74E-12	1.65E-10	down_regulated
stress	under	CvsDxD	TRINITY_DN748627_c0_g1	96.47240049	-23.69212006	3.396068771	-6.976336952	3.03E-12	2.56E-10	down_regulated
stress	under	CvsDxD	TRINITY_DN1025768_c0_g1	63.69494712	-9.700409939	3.40185185	-2.851508639	0.00435123	0.055975998	down_regulated
stress	under	CvsDxD	TRINITY_DN987461_c0_g1	46.59894018	-9.250524957	3.379770266	-2.737027735	0.006199707	0.064459963	down_regulated
stress	under	CvsDxD	TRINITY_DN1774784_c0_g1	40.49495351	-9.046852569	2.880155178	-3.141099007	0.001683151	0.026071669	down_regulated
stress	under	CvsDxD	TRINITY_DN3456_c3_g1	30.47822574	-8.638317649	3.381886118	-2.554289928	0.010640462	0.088322486	down_regulated
stress	under	CvsDxD	TRINITY_DN69870_c0_g1	27.45559178	-8.487735225	3.384387918	-2.5079085	0.012144811	0.092115437	down_regulated
stress	under	CvsDxD	TRINITY_DN1076453_c0_g1	24.43295783	-8.319582528	3.38832339	-2.455368503	0.014074023	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN59598_c0_g1	20.65466538	-8.077415341	3.396398402	-2.378229638	0.017395989	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1796678_c0_g1	49.81367809	-7.926621067	3.193790964	-2.481884744	0.013068954	0.094401759	down_regulated
stress	under	CvsDxD	TRINITY_DN1760029_c0_g1	17.63203142	-7.849355659	3.407003501	-2.303888345	0.021228912	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN14038_c0_g1	17.12825909	-7.807576885	3.409299225	-2.290082615	0.022016528	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN26363_c0_g1	41.15151845	-7.647042097	2.734988604	-2.796005104	0.005173858	0.062332674	down_regulated
stress	under	CvsDxD	TRINITY_DN606547_c0_g1	14.60939746	-7.578340223	3.424065157	-2.213258182	0.026879851	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1381219_c0_g1	14.41517224	-7.555494168	3.443818477	-2.193929273	0.028240495	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN459349_c0_g1	14.10562514	-7.527771649	3.427852087	-2.196060815	0.028087591	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN91406_c2_g1	13.60001785	-7.475435409	3.065806198	-2.438326145	0.014755454	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1647317_c0_g1	13.34996665	-7.4484327	3.43421015	-2.168892518	0.030090845	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1266_c0_g1	99.38774775	-7.40188771	2.829691241	-2.615793413	0.00890204	0.08043629	down_regulated
stress	under	CvsDxD	TRINITY_DN1698353_c0_g1	12.74449431	-7.378092023	2.995315128	-2.463210617	0.0137699	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN82726_c0_g1	10.83110502	-7.147174814	3.463432353	-2.063610339	0.039054676	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN100125_c0_g1	28.18463092	-7.013537743	3.189856252	-2.198700251	0.027899243	0.139462498	down_regulated
stress	under	CvsDxD	TRINITY_DN1273360_c0_g1	9.721860349	-6.986465861	3.500392276	-1.995909404	0.045943785	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN24911_c0_g1	9.642179871	-6.975253103	3.052171009	-2.28534151	0.022292811	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN105581_c0_g1	8.816015711	-6.850646213	3.501245129	-1.956631416	0.050390819	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN122919_c1_g1	8.816015711	-6.850646213	3.501245129	-1.956631416	0.050390819	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN384079_c0_g1	76.74542171	-6.829967064	3.044600681	-2.243304715	0.024877172	0.131374158	down_regulated
stress	under	CvsDxD	TRINITY_DN664299_c0_g1	8.382749055	-6.772283449	3.130320081	-2.163447595	0.03050677	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN40449_c2_g1	7.382544248	-6.592312873	3.09488751	-2.130065423	0.033166212	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1363751_c0_g1	6.965792245	-6.510436836	3.138570032	-2.074332186	0.038048471	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN434628_c0_g1	6.875101999	-6.486578612	3.146766583	-2.06134724	0.039269924	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN218074_c0_g1	6.630555681	-6.439704902	3.164536103	-2.034960162	0.041854882	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN43549_c2_g1	6.289814234	-6.359402584	3.151127786	-2.018135415	0.04357716	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN76832_c0_g1	6.121278471	-6.319464212	3.17020469	-1.993393118	0.046218417	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN265268_c0_g1	5.623011029	-6.201908878	3.211792354	-1.930980647	0.053485446	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1276013_c0_g1	5.539660628	-6.180957089	3.234097641	-1.91118444	0.055980883	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1785302_c0_g1	5.450805344	-6.15230036	3.206828526	-1.918499948	0.055047649	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN6633_c0_g2	5.369289904	-6.133223278	3.20351681	-1.914528202	0.055552699	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN19419_c0_g1	5.202589103	-6.089036342	3.226031089	-1.887469827	0.059097161	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN205816_c1_g1	12.88090584	-5.870880987	3.003865238	-1.954442201	0.050648944	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN14706_c0_g1	96.94190519	-5.777345279	3.032321458	-1.905254888	0.056746947	0.234081154	down_regulated
stress	under	CvsDxD	TRINITY_DN95445_c0_g1	21.19456431	-5.728104894	2.605826156	-2.198191496	0.027935462	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN39912_c0_g1	42.6478446	-5.64841651	2.765751164	-2.042272126	0.041124545	0.185794821	down_regulated
stress	under	CvsDxD	TRINITY_DN1587742_c0_g1	22.35362005	-5.633608193	2.605025025	-2.162592735	0.030572518	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN46812_c0_g1	9.702580844	-5.459533584	2.853555886	-1.913238711	0.0557175	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN61190_c0_g1	9.44885972	-5.418735924	2.874645842	-1.885009919	0.059428499	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN2871_c0_g2	26.07776873	-5.251644238	2.585772895	-2.030976598	0.042257369	0.189783093	down_regulated
stress	under	CvsDxD	TRINITY_DN12447_c0_g1	73.74284088	-5.059871682	2.198337091	-2.30168144	0.021353144	0.121857414	down_regulated
stress	under	CvsDxD	TRINITY_DN206411_c0_g1	19.63263992	-5.027213901	2.659034134	-1.890616535	0.058675551	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1947_c1_g1	30.77231813	-4.280941629	2.264158693	-1.890742748	0.058658693	0.238085284	down_regulated
stress	under	CvsDi	TRINITY_DN5665_c0_g1	49.28015048	4.051293028	2.089481205	1.938899004	0.052513637	0.298805667	up_regulated
stress	under	CvsDi	TRINITY_DN2753_c0_g1	65.71174451	4.487936659	2.093622163	2.143623017	0.032063105	0.210793251	up_regulated
stress	under	CvsDi	TRINITY_DN1032195_c0_g1	24.53704083	4.669223525	2.23091421	2.092964178	0.036352352	0.231181953	up_regulated
stress	under	CvsDi	TRINITY_DN114579_c0_g1	112.0537239	4.704472507	2.126017218	2.212810163	0.026910736	0.19395664	up_regulated
stress	under	CvsDi	TRINITY_DN46857_c0_g1	27.40456431	4.736882328	2.450682722	1.932882737	0.053250645	0.299496402	up_regulated
stress	under	CvsDi	TRINITY_DN39791_c4_g1	77.02086657	4.762283378	2.100494948	2.26721963	0.023376812	0.179099514	up_regulated
stress	under	CvsDi	TRINITY_DN18_c0_g1	250.0812769	5.066161357	2.178270745	2.325772114	0.020030711	0.164412176	up_regulated
stress	under	CvsDi	TRINITY_DN171895_c0_g2	18.96813369	5.077656532	2.651074624	1.915320107	0.055451693	0.306559643	up_regulated
stress	under	CvsDi	TRINITY_DN17614_c0_g1	11.61544448	5.166966057	2.731206555	1.891825445	0.058514242	NA	up_regulated
stress	under	CvsDi	TRINITY_DN3316_c0_g2	12.7763601	5.427836534	2.733612163	1.985591302	0.047078723	NA	up_regulated
stress	under	CvsDi	TRINITY_DN118144_c0_g1	14.42090135	5.487354008	2.739224892	2.003250637	0.045150393	NA	up_regulated
stress	under	CvsDi	TRINITY_DN198375_c0_g1	13.95406962	5.558608085	2.866751767	1.938991771	0.05250234	NA	up_regulated
stress	under	CvsDi	TRINITY_DN160_c5_g2	14.17712478	5.58078833	2.720898027	2.051083236	0.040258842	NA	up_regulated
stress	under	CvsDi	TRINITY_DN3316_c0_g1	29.34564492	5.584895492	2.369036074	2.357454812	0.018400695	0.162153655	up_regulated
stress	under	CvsDi	TRINITY_DN15147_c0_g1	33.30849709	5.636381757	2.676453874	2.105914027	0.035211821	0.227540233	up_regulated
stress	under	CvsDi	TRINITY_DN63653_c0_g1	36.91377179	5.796193078	2.414623279	2.400454401	0.016374731	0.157289673	up_regulated
stress	under	CvsDi	TRINITY_DN473108_c0_g1	6.346750012	5.852237493	3.083232869	1.898084816	0.05768491	NA	up_regulated
stress	under	CvsDi	TRINITY_DN6633_c0_g1	36.85925062	5.919448064	2.465390823	2.40101813	0.016349526	0.157289673	up_regulated
stress	under	CvsDi	TRINITY_DN74308_c0_g1	7.154732423	6.025162693	3.03337604	1.986289406	0.047001198	NA	up_regulated
stress	under	CvsDi	TRINITY_DN400370_c0_g1	7.40011437	6.073687776	3.03909625	1.998517743	0.045660558	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1270872_c0_g1	8.011915048	6.188806296	3.180391438	1.94592597	0.051663633	NA	up_regulated
stress	under	CvsDi	TRINITY_DN39791_c3_g1	8.117551366	6.207474451	2.991450093	2.075072041	0.037979859	NA	up_regulated
stress	under	CvsDi	TRINITY_DN12081_c0_g1	8.506296888	6.274541452	3.168113524	1.980529235	0.047644092	NA	up_regulated
stress	under	CvsDi	TRINITY_DN47809_c0_g1	26.15189199	6.346636634	3.092201364	2.052465505	0.040124444	0.247095469	up_regulated
stress	under	CvsDi	TRINITY_DN310421_c0_g1	9.605243516	6.450380361	3.416236512	1.88815392	0.05900529	NA	up_regulated
stress	under	CvsDi	TRINITY_DN2082_c0_g1	29.10794832	6.524259638	2.598486559	2.510792144	0.01204606	0.136377219	up_regulated
stress	under	CvsDi	TRINITY_DN1264070_c0_g1	10.18655283	6.534937802	2.918014904	2.239514882	0.025122434	NA	up_regulated
stress	under	CvsDi	TRINITY_DN33742_c0_g1	10.60578972	6.593318529	3.392884269	1.943278345	0.051982536	NA	up_regulated
stress	under	CvsDi	TRINITY_DN74_c1_g2	10.78295304	6.616744642	3.064558274	2.159118558	0.03084097	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1112984_c0_g1	11.0060082	6.646750681	3.384828052	1.963689316	0.049566132	NA	up_regulated
stress	under	CvsDi	TRINITY_DN357595_c0_g1	33.22362445	6.711981855	2.709152978	2.477520431	0.013229883	0.144015439	up_regulated
stress	under	CvsDi	TRINITY_DN1784945_c0_g1	11.99508144	6.770747951	2.878117795	2.352491605	0.018648111	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1596896_c0_g1	12.63706392	6.846147191	2.983678814	2.294532226	0.021759946	NA	up_regulated
stress	under	CvsDi	TRINITY_DN42134_c0_g2	12.80699135	6.865363465	3.355387784	2.046071545	0.040749332	NA	up_regulated
stress	under	CvsDi	TRINITY_DN121577_c0_g1	13.02942739	6.889827104	2.940068303	2.343424164	0.019107646	NA	up_regulated
stress	under	CvsDi	TRINITY_DN71348_c1_g1	13.49755493	6.940754237	3.348813179	2.072601207	0.038209409	NA	up_regulated
stress	under	CvsDi	TRINITY_DN134731_c0_g1	13.61497377	6.953664049	3.080630507	2.257221056	0.023994265	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1324417_c0_g1	14.40786527	7.035268874	3.336089379	2.108837046	0.03495865	NA	up_regulated
stress	under	CvsDi	TRINITY_DN301_c0_g1	14.43804708	7.038339302	2.966196438	2.372850028	0.017651429	NA	up_regulated
stress	under	CvsDi	TRINITY_DN13449_c0_g2	14.80808375	7.07479275	3.332011991	2.123279499	0.033730438	NA	up_regulated
stress	under	CvsDi	TRINITY_DN76336_c0_g1	15.01573844	7.094927044	3.068702705	2.312028152	0.020776132	NA	up_regulated
stress	under	CvsDi	TRINITY_DN33577_c0_g3	15.20830223	7.11326271	3.328185231	2.137279693	0.03257525	NA	up_regulated
stress	under	CvsDi	TRINITY_DN89748_c0_g1	15.54392003	7.144412744	2.960588027	2.413173558	0.015814287	NA	up_regulated
stress	under	CvsDi	TRINITY_DN223639_c0_g1	15.98217533	7.184822267	2.833140109	2.53599257	0.011212911	NA	up_regulated
stress	under	CvsDi	TRINITY_DN78823_c0_g1	16.6123753	7.24034378	3.318862129	2.181574136	0.029140977	0.198280917	up_regulated
stress	under	CvsDi	TRINITY_DN1068510_c0_g1	16.60906691	7.240361288	3.316496407	2.183135574	0.029025827	0.198280917	up_regulated
stress	under	CvsDi	TRINITY_DN480450_c0_g1	17.02768468	7.275970948	3.315838302	2.194308131	0.028213265	0.197490173	up_regulated
stress	under	CvsDi	TRINITY_DN106037_c0_g1	17.20939463	7.291581464	3.312181805	2.201443608	0.027704635	0.197490173	up_regulated
stress	under	CvsDi	TRINITY_DN89774_c0_g1	180.465259	7.298554241	2.430490506	3.002914113	0.002674079	0.068643986	up_regulated
stress	under	CvsDi	TRINITY_DN1320548_c0_g1	18.68892221	7.410283147	3.305370232	2.24189202	0.024968352	0.185451957	up_regulated
stress	under	CvsDi	TRINITY_DN2512_c0_g3	19.21048703	7.450263985	3.300160604	2.257545883	0.023973986	0.180165443	up_regulated
stress	under	CvsDi	TRINITY_DN115111_c0_g1	19.41783216	7.465613414	2.78699186	2.678735277	0.007390079	0.112025547	up_regulated
stress	under	CvsDi	TRINITY_DN64994_c0_g1	20.35015974	7.533149375	3.297006921	2.284844878	0.022321925	0.173152754	up_regulated
stress	under	CvsDi	TRINITY_DN10234_c0_g1	20.98097882	7.577350473	2.773567409	2.731987133	0.00629536	0.10561009	up_regulated
stress	under	CvsDi	TRINITY_DN1258812_c0_g1	21.81190715	7.633469615	3.28861091	2.321183571	0.020276938	0.164412176	up_regulated
stress	under	CvsDi	TRINITY_DN75796_c0_g1	22.41223487	7.672636927	3.286440035	2.334634694	0.019562513	0.164412176	up_regulated
stress	under	CvsDi	TRINITY_DN176635_c0_g1	24.21321803	7.784136243	3.280795147	2.372637088	0.017661608	0.16212023	up_regulated
stress	under	CvsDi	TRINITY_DN690549_c0_g1	180.2255498	7.786396449	2.528859366	3.079015209	0.002076861	0.063149548	up_regulated
stress	under	CvsDi	TRINITY_DN1024355_c0_g1	135.3618873	7.81387946	2.361141931	3.309364574	0.00093508	0.036166987	up_regulated
stress	under	CvsDi	TRINITY_DN71348_c0_g1	24.91856294	7.825349772	3.281258881	2.384862047	0.017085527	0.159848247	up_regulated
stress	under	CvsDi	TRINITY_DN1105384_c0_g1	26.26681903	7.901511216	2.745317025	2.878178055	0.003999793	0.084203407	up_regulated
stress	under	CvsDi	TRINITY_DN540229_c0_g1	28.41551207	8.015002971	3.271423999	2.450004333	0.01428545	0.148233181	up_regulated
stress	under	CvsDi	TRINITY_DN1874576_c0_g1	32.21758763	8.196160574	3.266019073	2.509526243	0.012089323	0.136377219	up_regulated
stress	under	CvsDi	TRINITY_DN2512_c0_g1	33.61835231	8.257557285	3.264536374	2.529473205	0.01142339	0.132698631	up_regulated
stress	under	CvsDi	TRINITY_DN2512_c0_g2	34.81900774	8.308180204	3.26343815	2.545836575	0.010901621	0.132698631	up_regulated
stress	under	CvsDi	TRINITY_DN171_c0_g2	35.233698	8.32518263	2.723312189	3.057006341	0.002235595	0.065916194	up_regulated
stress	under	CvsDi	TRINITY_DN1263362_c0_g1	35.44197182	8.333800898	2.937917028	2.836635895	0.004559157	0.089020367	up_regulated
stress	under	CvsDi	TRINITY_DN6035_c1_g1	94.70672845	8.341228282	2.527553125	3.300119867	0.000966435	0.036166987	up_regulated
stress	under	CvsDi	TRINITY_DN1411047_c0_g1	36.21977242	8.365079188	3.262332777	2.564140374	0.01034317	0.132419799	up_regulated
stress	under	CvsDi	TRINITY_DN11599_c0_g1	38.02830103	8.435399818	3.019549293	2.793595666	0.005212561	0.094615278	up_regulated
stress	under	CvsDi	TRINITY_DN1735_c0_g1	40.70031947	8.533202136	3.262134498	2.61583394	0.008900984	0.119167658	up_regulated
stress	under	CvsDi	TRINITY_DN649497_c0_g1	41.8605584	8.573920888	2.899409351	2.957126729	0.003105205	0.075534103	up_regulated
stress	under	CvsDi	TRINITY_DN26933_c0_g1	42.17684823	8.584658009	2.730057328	3.144497342	0.001663724	0.055299736	up_regulated
stress	under	CvsDi	TRINITY_DN6750_c0_g1	129.0080413	8.633527988	3.099704551	2.785274482	0.005348243	0.094615278	up_regulated
stress	under	CvsDi	TRINITY_DN54792_c1_g1	45.06985169	8.680486568	2.886275738	3.007504257	0.002634024	0.068643986	up_regulated
stress	under	CvsDi	TRINITY_DN5340_c3_g1	45.22468822	8.685394764	3.258421307	2.665522333	0.007686883	0.113323284	up_regulated
stress	under	CvsDi	TRINITY_DN171_c1_g1	55.41062192	8.978421416	2.68471852	3.344269185	0.000824996	0.034900938	up_regulated
stress	under	CvsDi	TRINITY_DN19883_c0_g2	57.48417001	9.031470451	2.878725978	3.13731509	0.001705028	0.055299736	up_regulated
stress	under	CvsDi	TRINITY_DN20029_c0_g1	58.44698896	9.055425127	2.970902898	3.048038067	0.002303407	0.065918102	up_regulated
stress	under	CvsDi	TRINITY_DN1250314_c0_g1	76.65692979	9.446695236	2.977537392	3.172653771	0.001510525	0.052490759	up_regulated
stress	under	CvsDi	TRINITY_DN655801_c0_g1	98.62843287	9.810200427	3.037167717	3.23004896	0.00123769	0.044602688	up_regulated
stress	under	CvsDi	TRINITY_DN951000_c0_g1	85.84686392	20.85045914	3.264090792	6.387830628	1.68E-10	1.09E-08	up_regulated
stress	under	CvsDr	TRINITY_DN74_c0_g1	443.684454	4.342444429	2.186076647	1.986409962	0.046987821	0.236241366	up_regulated
stress	under	CvsDr	TRINITY_DN1068_c0_g1	213.8330573	4.676666467	2.458643666	1.902132681	0.057153811	0.263545208	up_regulated
stress	under	CvsDr	TRINITY_DN295_c0_g1	1360.227823	4.963187145	2.520830873	1.96886955	0.048968073	0.239003112	up_regulated
stress	under	CvsDr	TRINITY_DN274_c0_g1	1013.004819	5.015647111	2.571221982	1.950686151	0.051094392	0.245333982	up_regulated
stress	under	CvsDr	TRINITY_DN13622_c0_g1	73.99546446	5.240224711	2.510541213	2.087288862	0.036862031	0.20426554	up_regulated
stress	under	CvsDr	TRINITY_DN19883_c0_g1	65.67440696	5.29578418	2.801406271	1.890402058	0.058704208	0.267067911	up_regulated
stress	under	CvsDr	TRINITY_DN13270_c0_g1	44.74927197	5.487954215	2.768414784	1.982345365	0.0474406	0.236241366	up_regulated
stress	under	CvsDr	TRINITY_DN3316_c0_g1	28.90317125	5.491704713	2.637855506	2.08188231	0.037353221	0.205442716	up_regulated
stress	under	CvsDr	TRINITY_DN2753_c0_g1	163.9570044	5.714035873	2.303282494	2.480822864	0.013107949	0.113624576	up_regulated
stress	under	CvsDr	TRINITY_DN735_c0_g1	221.8021245	5.804567422	2.97782405	1.94926474	0.051263817	0.245333982	up_regulated
stress	under	CvsDr	TRINITY_DN184629_c0_g1	39.5942057	5.907518986	2.9228246	2.021167807	0.043262397	0.22760957	up_regulated
stress	under	CvsDr	TRINITY_DN11435_c1_g1	7.67434719	6.057120656	3.187215192	1.900442955	0.057375014	NA	up_regulated
stress	under	CvsDr	TRINITY_DN6035_c0_g3	127.6859193	6.119541605	2.711635018	2.256771861	0.024022334	0.156676638	up_regulated
stress	under	CvsDr	TRINITY_DN135651_c0_g1	8.048749061	6.121145499	3.197634067	1.914273294	0.055585245	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1486_c0_g1	225.9567295	6.125021803	2.916646722	2.100021836	0.03572692	0.204114266	up_regulated
stress	under	CvsDr	TRINITY_DN1038034_c0_g1	8.535130234	6.205837551	3.1862299	1.947705516	0.05145021	NA	up_regulated
stress	under	CvsDr	TRINITY_DN16791_c0_g1	124.0703944	6.344848367	3.186139623	1.99139056	0.046437966	0.236241366	up_regulated
stress	under	CvsDr	TRINITY_DN1151_c0_g2	169.6584469	6.49598125	2.925113719	2.220761951	0.026367089	0.163947225	up_regulated
stress	under	CvsDr	TRINITY_DN56871_c0_g1	10.84864614	6.551686587	3.315567428	1.976037806	0.048150496	NA	up_regulated
stress	under	CvsDr	TRINITY_DN6035_c0_g2	236.6791914	6.57165289	2.46503864	2.665943155	0.007677267	0.092151384	up_regulated
stress	under	CvsDr	TRINITY_DN1773020_c0_g1	370.9716591	6.668232803	2.991982594	2.228700399	0.025833846	0.163947225	up_regulated
stress	under	CvsDr	TRINITY_DN74308_c0_g1	11.85550656	6.683966956	3.087519953	2.164833607	0.03040043	NA	up_regulated
stress	under	CvsDr	TRINITY_DN160_c5_g2	34.59034046	6.786919432	3.332765067	2.036422999	0.041707898	0.221141876	up_regulated
stress	under	CvsDr	TRINITY_DN4557_c0_g1	429.2263218	6.799610222	2.306525672	2.947988095	0.003198494	0.058932249	up_regulated
stress	under	CvsDr	TRINITY_DN289592_c0_g1	13.34342897	6.855160331	3.55555486	1.92801422	0.053853357	NA	up_regulated
stress	under	CvsDr	TRINITY_DN2512_c0_g1	13.49168929	6.871081383	3.55425423	1.933199186	0.053211665	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1085410_c0_g1	13.86186344	6.906225286	3.581560433	1.928272722	0.053821213	NA	up_regulated
stress	under	CvsDr	TRINITY_DN91_c5_g1	797.5433346	6.950507892	2.723640252	2.55191848	0.010713158	0.099944273	up_regulated
stress	under	CvsDr	TRINITY_DN1617615_c0_g1	14.34824462	6.956038248	3.577775649	1.94423545	0.051867065	NA	up_regulated
stress	under	CvsDr	TRINITY_DN3316_c0_g2	39.4541522	6.978397921	3.332945955	2.0937627	0.036281124	0.204115944	up_regulated
stress	under	CvsDr	TRINITY_DN29317_c0_g1	40.96900425	6.99090828	2.913714063	2.399311713	0.016425925	0.122281886	up_regulated
stress	under	CvsDr	TRINITY_DN10912_c0_g1	402.6072202	6.99116749	2.712700856	2.577198099	0.009960485	0.099501893	up_regulated
stress	under	CvsDr	TRINITY_DN6035_c1_g1	40.42691455	7.013822916	3.333081102	2.104306106	0.035351754	0.20354877	up_regulated
stress	under	CvsDr	TRINITY_DN2512_c0_g2	15.27081315	7.049570604	3.541046899	1.990815373	0.046501189	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1299382_c0_g1	15.56419755	7.073529051	3.569588283	1.98160922	0.047522994	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1360971_c0_g1	16.29376931	7.13968991	3.565412833	2.002486176	0.045232468	NA	up_regulated
stress	under	CvsDr	TRINITY_DN7122_c1_g1	17.26653166	7.223434991	3.560551883	2.028740271	0.042484752	NA	up_regulated
stress	under	CvsDr	TRINITY_DN22891_c0_g1	18.9773212	7.362738404	3.523347349	2.089699843	0.036644771	NA	up_regulated
stress	under	CvsDr	TRINITY_DN471033_c0_g1	19.21205635	7.377616957	3.552763251	2.076585586	0.037839824	NA	up_regulated
stress	under	CvsDr	TRINITY_DN11041_c0_g1	20.67119987	7.483322317	3.54822996	2.109029686	0.034942019	NA	up_regulated
stress	under	CvsDr	TRINITY_DN144174_c0_g1	20.91439046	7.500209599	3.54756284	2.114186538	0.034499338	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1351966_c0_g1	21.40077163	7.533404501	3.546295907	2.124302285	0.033644875	NA	up_regulated
stress	under	CvsDr	TRINITY_DN43266_c0_g1	115.0189012	7.557896659	2.839210436	2.661971287	0.00776845	0.092151384	up_regulated
stress	under	CvsDr	TRINITY_DN63509_c0_g1	22.61672457	7.613196809	3.54348411	2.148505982	0.031673582	NA	up_regulated
stress	under	CvsDr	TRINITY_DN10478_c0_g1	23.81658176	7.690393767	3.228656265	2.381917781	0.017222742	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1822818_c0_g1	25.53501161	7.788415301	3.538395605	2.201114903	0.02772789	0.166141913	up_regulated
stress	under	CvsDr	TRINITY_DN655801_c0_g1	27.99883261	7.923093135	2.981648472	2.657286132	0.007877255	0.092151384	up_regulated
stress	under	CvsDr	TRINITY_DN2123_c1_g1	387.0113714	7.950682715	2.448039759	3.247775158	0.001163111	0.031748634	up_regulated
stress	under	CvsDr	TRINITY_DN1266662_c0_g1	1697.689768	7.991704801	2.871640373	2.782975499	0.005386287	0.076888617	up_regulated
stress	under	CvsDr	TRINITY_DN39791_c3_g1	30.15563276	8.028524321	3.53359241	2.272057269	0.023083052	0.151894723	up_regulated
stress	under	CvsDr	TRINITY_DN7172_c0_g1	30.88520452	8.063033976	3.533088058	2.282149169	0.022480535	0.149921179	up_regulated
stress	under	CvsDr	TRINITY_DN171_c1_g1	31.37158569	8.085589138	3.532781979	2.288731427	0.022094962	0.149394375	up_regulated
stress	under	CvsDr	TRINITY_DN1352798_c0_g1	31.8759692	8.11035082	3.502720672	2.315443217	0.020588689	0.141046167	up_regulated
stress	under	CvsDr	TRINITY_DN21575_c0_g1	362.569356	8.141296413	2.793311536	2.914567999	0.003561812	0.064025743	up_regulated
stress	under	CvsDr	TRINITY_DN26933_c0_g1	33.31711038	8.172443418	3.531771757	2.31397836	0.020668909	0.141046167	up_regulated
stress	under	CvsDr	TRINITY_DN219267_c0_g1	35.01944449	8.244375086	3.531129181	2.334770172	0.01955543	0.139925748	up_regulated
stress	under	CvsDr	TRINITY_DN1500_c0_g1	103.1672834	8.327919895	2.982461342	2.792297683	0.005233518	0.076888617	up_regulated
stress	under	CvsDr	TRINITY_DN399689_c0_g2	200.7467884	8.367989439	3.259937413	2.566917207	0.010260711	0.099501893	up_regulated
stress	under	CvsDr	TRINITY_DN42725_c0_g1	40.91984883	8.470485744	3.500676634	2.419671003	0.015534555	0.118030586	up_regulated
stress	under	CvsDr	TRINITY_DN1798227_c0_g1	44.32983624	8.585909069	3.500829924	2.452535329	0.014185345	0.113781382	up_regulated
stress	under	CvsDr	TRINITY_DN61618_c0_g1	46.55374106	8.65649709	3.501093142	2.472512652	0.013416696	0.113624576	up_regulated
stress	under	CvsDr	TRINITY_DN834185_c0_g1	47.42216442	8.681980049	3.530524964	2.459118725	0.013927855	0.113781382	up_regulated
stress	under	CvsDr	TRINITY_DN25330_c0_g2	50.34045146	8.768168857	3.53098736	2.48320596	0.013020578	0.113624576	up_regulated
stress	under	CvsDr	TRINITY_DN17244_c0_g1	52.78067458	8.837536228	3.502306768	2.523347272	0.011624354	0.105767273	up_regulated
stress	under	CvsDr	TRINITY_DN76160_c0_g2	56.17702554	8.926486007	3.532259013	2.527132346	0.011499816	0.105767273	up_regulated
stress	under	CvsDr	TRINITY_DN50476_c0_g1	61.67629389	9.062162529	3.504773407	2.585662888	0.009719194	0.099501893	up_regulated
stress	under	CvsDr	TRINITY_DN5125_c0_g1	65.67932259	9.152856596	3.506036197	2.610599572	0.009038366	0.099501893	up_regulated
stress	under	CvsDr	TRINITY_DN223639_c0_g1	66.63422077	9.172843165	3.535204307	2.594713733	0.009466973	0.099501893	up_regulated
stress	under	CvsDr	TRINITY_DN10234_c0_g1	76.24768199	9.367289938	2.989170009	3.133742781	0.001725921	0.037147325	up_regulated
stress	under	CvsDr	TRINITY_DN39791_c4_g1	2287.765707	9.554982726	2.803735262	3.407947553	0.000654535	0.021156092	up_regulated
stress	under	CvsDr	TRINITY_DN301_c0_g1	102.2409728	9.79127343	3.05988482	3.199883004	0.001374834	0.033775085	up_regulated
stress	under	CvsDr	TRINITY_DN171_c3_g1	150.5258269	10.34916638	3.218548684	3.215476102	0.001302283	0.033775085	up_regulated
stress	under	CvsDr	TRINITY_DN171_c0_g1	194.3028682	10.71705211	3.115141354	3.440310052	0.000581048	0.021156092	up_regulated
stress	under	CvsDr	TRINITY_DN716478_c0_g1	196.2849363	10.7320508	3.20404761	3.349529128	0.00080949	0.024858102	up_regulated
stress	under	CvsDr	TRINITY_DN60_c7_g1	674.2086514	11.02473977	3.435493394	3.209070285	0.001331649	0.033775085	up_regulated
stress	under	CvsDr	TRINITY_DN976820_c0_g1	126.4660545	21.4769917	3.526127441	6.090815507	1.12E-09	4.87E-08	up_regulated
stress	under	CvsDr	TRINITY_DN1273607_c0_g1	94.60113825	21.83638285	3.544460074	6.160707807	7.24E-10	3.34E-08	up_regulated
stress	under	CvsDr	TRINITY_DN511505_c0_g1	98.88963468	22.48002484	3.517261622	6.391342828	1.64E-10	8.34E-09	up_regulated
stress	under	CvsDr	TRINITY_DN17244_c1_g1	99.92745694	22.49471155	3.517605127	6.394893894	1.61E-10	8.34E-09	up_regulated
stress	under	CvsDr	TRINITY_DN61290_c0_g1	113.8131946	22.67524922	3.550541308	6.386420338	1.70E-10	8.34E-09	up_regulated
stress	under	CvsDr	TRINITY_DN160_c2_g2	119.1633875	22.74064663	3.552161095	6.401918725	1.53E-10	8.34E-09	up_regulated
stress	under	CvsDr	TRINITY_DN161957_c0_g1	151.5077355	23.07479638	3.561221999	6.479460248	9.21E-11	7.54E-09	up_regulated
stress	under	CvsDr	TRINITY_DN1762827_c0_g1	162.2081214	23.16461142	3.563956141	6.499690371	8.05E-11	7.41E-09	up_regulated
stress	under	CvsDr	TRINITY_DN1174_c0_g1	178.0606466	23.29858713	3.539999259	6.581523166	4.66E-11	4.90E-09	up_regulated
stress	under	CvsDr	TRINITY_DN60096_c1_g1	330.3239971	24.14227824	3.568211896	6.765931773	1.32E-11	1.95E-09	up_regulated
stress	under	CvsDr	TRINITY_DN171_c0_g2	336.0893909	24.17620673	3.5956839	6.723674105	1.77E-11	2.18E-09	up_regulated
stress	under	CvsDxD	TRINITY_DN13622_c0_g1	46.25063	4.601254899	2.215498182	2.076848872	0.03781551	0.174901395	up_regulated
stress	under	CvsDxD	TRINITY_DN6035_c3_g1	40.32887462	4.926810991	2.53234308	1.945554309	0.0517083	0.224317828	up_regulated
stress	under	CvsDxD	TRINITY_DN43266_c0_g1	16.64858047	4.943878322	2.622437071	1.885222863	0.059399755	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN5340_c0_g1	252.7960716	5.02209796	2.239441522	2.242567136	0.024924741	0.131374158	up_regulated
stress	under	CvsDxD	TRINITY_DN24919_c0_g1	52.41290844	5.084154982	2.421862728	2.09927463	0.035792699	0.166666615	up_regulated
stress	under	CvsDxD	TRINITY_DN3456_c0_g1	1568.784974	5.32672457	2.399935582	2.219528145	0.026450815	0.136572574	up_regulated
stress	under	CvsDxD	TRINITY_DN39791_c4_g1	131.7361002	5.621973129	2.474040926	2.272384854	0.023063276	0.125935441	up_regulated
stress	under	CvsDxD	TRINITY_DN121_c0_g1	310.9847259	5.755440865	2.311696655	2.489704198	0.012784945	0.094211393	up_regulated
stress	under	CvsDxD	TRINITY_DN6035_c0_g2	127.2756634	5.833141207	2.272927971	2.566355503	0.010277344	0.086672264	up_regulated
stress	under	CvsDxD	TRINITY_DN129372_c0_g1	18.09127865	5.842821086	2.838470824	2.058439719	0.039547942	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN6750_c0_g1	20.70512814	6.042062875	2.844130175	2.124397445	0.033636924	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1113321_c0_g1	7.06214004	6.045643289	3.102398269	1.94869993	0.05133127	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1439974_c0_g1	7.168501889	6.066225651	3.130582501	1.937730646	0.052656093	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN45506_c1_g1	7.234827622	6.079340864	3.146459234	1.932121287	0.053344538	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN110134_c0_g1	21.73558377	6.094703252	3.222077772	1.891544427	0.058551706	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN446864_c0_g1	7.32744297	6.098041405	3.112838012	1.958997346	0.050113098	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN5340_c2_g1	21.96085058	6.109829821	3.221699115	1.896461961	0.057898985	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN160_c5_g2	20.03859265	6.139723159	3.218515265	1.907625925	0.056439582	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN78823_c0_g1	7.652198381	6.160356279	3.124169271	1.971838189	0.048628081	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1773020_c0_g1	223.7841104	6.182082296	3.101406722	1.993315566	0.046226903	0.205484948	up_regulated
stress	under	CvsDxD	TRINITY_DN159361_c0_g1	10.21467203	6.579264992	3.207872622	2.050974514	0.040269429	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1274526_c0_g1	10.4880516	6.615153742	3.130255417	2.113295199	0.034575509	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN156101_c0_g1	10.61949968	6.634479727	3.002961324	2.209312412	0.027152918	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1454_c0_g1	329.9058936	6.650185947	2.901935073	2.291638434	0.021926516	0.123275744	up_regulated
stress	under	CvsDxD	TRINITY_DN1078665_c0_g1	10.757821	6.653778121	3.487795603	1.907731667	0.056425907	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1266662_c0_g1	581.4068192	6.673090297	2.930547804	2.277079489	0.022781479	0.125935441	up_regulated
stress	under	CvsDxD	TRINITY_DN6843_c0_g2	10.94992495	6.679298814	3.485000315	1.916584853	0.055290694	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN296853_c0_g1	11.03807382	6.68907816	3.493820795	1.914545294	0.055550518	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN59871_c0_g1	11.1420289	6.70437581	3.48231651	1.925263195	0.05419644	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN947215_c0_g1	11.20268477	6.711340909	2.988780616	2.245511388	0.024735322	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN945474_c0_g1	282.8806446	6.718002292	2.885297142	2.328357171	0.019893146	0.11833696	up_regulated
stress	under	CvsDxD	TRINITY_DN972457_c0_g1	11.26334063	6.718240215	3.490741753	1.92458815	0.054280903	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN977329_c0_g1	11.28963025	6.72154647	3.070766634	2.188882214	0.028605402	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN174364_c1_g1	11.33413284	6.729024269	3.479738381	1.933773041	0.053141039	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN168500_c0_g1	11.48860745	6.746824559	3.487803055	1.934405255	0.053063321	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN29317_c0_g1	33.5841692	6.755383305	2.709827529	2.492920022	0.012669738	0.094211393	up_regulated
stress	under	CvsDxD	TRINITY_DN64994_c0_g1	11.71834074	6.777092498	3.474877819	1.950311019	0.05113906	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN267925_c0_g1	11.91044468	6.800539293	3.472585577	1.958350383	0.050188913	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN5665_c0_g1	305.6694255	6.812196895	2.843353826	2.395831582	0.016582708	0.104885627	up_regulated
stress	under	CvsDxD	TRINITY_DN1622692_c0_g1	12.16440789	6.829329015	3.479748963	1.962592442	0.04969355	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN237246_c0_g1	12.16440789	6.829329015	3.479748963	1.962592442	0.04969355	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN3479_c1_g1	12.3896747	6.855814445	3.477294488	1.97159443	0.048655924	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN111551_c1_g1	12.61494151	6.881822449	3.474944512	1.980412184	0.047657232	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN295380_c0_g1	12.61494151	6.881822449	3.474944512	1.980412184	0.047657232	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN876088_c0_g1	12.61494151	6.881822449	3.474944512	1.980412184	0.047657232	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN90847_c0_g2	12.84020832	6.907369935	3.472693336	1.989052665	0.04669539	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN660589_c0_g1	13.06547514	6.932472926	3.470535658	1.99752246	0.045768457	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN658105_c0_g1	13.29074195	6.957146626	3.468466537	2.005827806	0.044874623	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN26933_c0_g1	13.44727626	6.9755378	3.457024297	2.017786744	0.043613476	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1863013_c0_g1	13.6393802	6.995992086	3.455375598	2.024669067	0.042901357	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN20873_c0_g2	13.74127557	7.00526319	3.46457583	2.021968499	0.043179606	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1320548_c0_g1	14.40779599	7.075026901	3.449320795	2.051136244	0.040253681	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN78405_c0_g1	14.79887713	7.113754462	3.032505627	2.345833887	0.018984566	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1911001_c0_g1	15.02655072	7.134190277	3.124342259	2.283421496	0.022405552	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN138189_c0_g1	40.57464564	7.165890271	3.217490809	2.227167286	0.025936096	0.135762045	up_regulated
stress	under	CvsDxD	TRINITY_DN23303_c0_g1	190.9362017	7.170412868	2.585182484	2.773658306	0.005542986	0.063744344	up_regulated
stress	under	CvsDxD	TRINITY_DN1394900_c0_g1	76.29221585	7.18078669	2.793772741	2.570283039	0.010161545	0.086658573	up_regulated
stress	under	CvsDxD	TRINITY_DN112686_c0_g1	15.54341008	7.183127359	3.451764979	2.081001285	0.037433788	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN5125_c0_g1	16.71304335	7.289059407	3.435280957	2.121823367	0.033852574	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1128154_c0_g1	17.12027776	7.322585773	3.443321065	2.126605575	0.033452869	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN791028_c0_g1	17.32251805	7.340844368	3.157936302	2.32457012	0.020094958	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN6035_c1_g1	45.84684722	7.341119027	2.931013639	2.504634891	0.012257785	0.092115437	up_regulated
stress	under	CvsDxD	TRINITY_DN662825_c0_g1	17.7960782	7.378459531	3.440302074	2.144712695	0.031975825	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN71219_c0_g1	17.86566702	7.385236856	3.430003062	2.153128357	0.031308592	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN797637_c0_g1	18.02134502	7.396613488	3.439363961	2.15057597	0.031509684	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN112443_c0_g1	18.47187864	7.432250164	3.437582003	2.162057562	0.03061374	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1577083_c0_g1	54.42556123	7.448867607	2.86489731	2.600046983	0.0093211	0.083231938	up_regulated
stress	under	CvsDxD	TRINITY_DN2123_c1_g1	273.8101341	7.575479796	2.313176634	3.274924916	0.001056901	0.020054689	up_regulated
stress	under	CvsDxD	TRINITY_DN1452574_c0_g1	20.49927996	7.582543633	3.430897534	2.210075806	0.027099902	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN182167_c0_g1	20.49927996	7.582543633	3.430897534	2.210075806	0.027099902	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1585434_c0_g1	21.85088083	7.67469072	3.427423878	2.239200926	0.025142846	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN681921_c0_g1	24.32881577	7.829710265	3.42256078	2.287676032	0.022156394	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN93451_c0_g1	25.39329058	7.891916556	2.875938558	2.744118624	0.006067361	0.064459963	up_regulated
stress	under	CvsDxD	TRINITY_DN171_c1_g1	29.04085879	8.085998957	3.141658442	2.573799509	0.010058854	0.086658573	up_regulated
stress	under	CvsDxD	TRINITY_DN171_c0_g2	32.27346301	8.238136978	3.405029283	2.419402682	0.01554602	0.099995162	up_regulated
stress	under	CvsDxD	TRINITY_DN760950_c0_g1	32.88895465	8.264751283	3.414560818	2.420443426	0.015501591	0.099995162	up_regulated
stress	under	CvsDxD	TRINITY_DN161957_c0_g1	34.01528872	8.313341458	3.414113613	2.434992622	0.014892093	0.099995162	up_regulated
stress	under	CvsDxD	TRINITY_DN9571_c0_g1	34.48403532	8.333201675	2.874507638	2.899001403	0.003743532	0.049848087	up_regulated
stress	under	CvsDxD	TRINITY_DN134731_c0_g1	34.96291826	8.353588283	3.404021096	2.454035403	0.014126307	0.096593399	up_regulated
stress	under	CvsDxD	TRINITY_DN12342_c0_g1	36.49974984	8.415636033	3.403661512	2.472524369	0.013416257	0.094401759	up_regulated
stress	under	CvsDxD	TRINITY_DN1075806_c0_g1	36.71849047	8.423685762	3.41338749	2.467837533	0.0135932	0.094653567	up_regulated
stress	under	CvsDxD	TRINITY_DN1617615_c0_g1	37.26816562	8.44568704	3.403530968	2.481448566	0.013084959	0.094401759	up_regulated
stress	under	CvsDxD	TRINITY_DN15248_c0_g1	40.32275947	8.558797042	3.413009393	2.507698062	0.012152046	0.092115437	up_regulated
stress	under	CvsDxD	TRINITY_DN664347_c0_g1	42.83918007	8.646635385	3.403340858	2.540631617	0.011065244	0.088322486	up_regulated
stress	under	CvsDxD	TRINITY_DN1915286_c0_g1	43.47649485	8.667455732	3.413080668	2.539481652	0.011101687	0.088322486	up_regulated
stress	under	CvsDxD	TRINITY_DN1500_c0_g1	144.0831182	8.869964269	2.659444073	3.335270089	0.000852166	0.017020892	up_regulated
stress	under	CvsDxD	TRINITY_DN56714_c0_g1	50.14600329	8.873885046	3.014214392	2.944012566	0.003239868	0.044714478	up_regulated
stress	under	CvsDxD	TRINITY_DN4557_c0_g1	1782.612177	8.918451331	2.425257162	3.677321924	0.000235696	0.006389032	up_regulated
stress	under	CvsDxD	TRINITY_DN33894_c0_g1	152.6843194	9.081455219	2.887792613	3.144774032	0.001662152	0.026071669	up_regulated
stress	under	CvsDxD	TRINITY_DN509816_c0_g1	187.4656322	9.233403111	2.993771316	3.084204549	0.002040972	0.030981958	up_regulated
stress	under	CvsDxD	TRINITY_DN11435_c1_g1	67.12951018	9.294244378	3.418965694	2.718437449	0.006559106	0.067275151	up_regulated
stress	under	CvsDxD	TRINITY_DN42134_c0_g3	72.08538006	9.397013591	3.42064039	2.747150393	0.006011556	0.064459963	up_regulated
stress	under	CvsDxD	TRINITY_DN14462_c0_g1	75.61645008	9.465997548	3.010019192	3.144829632	0.001661836	0.026071669	up_regulated
stress	under	CvsDxD	TRINITY_DN1610597_c0_g1	87.19767288	9.671609546	2.989687904	3.23498969	0.001216471	0.021983373	up_regulated
stress	under	CvsDxD	TRINITY_DN16256_c0_g1	117.1249961	10.09733219	2.998691527	3.367246047	0.000759229	0.015574458	up_regulated
stress	under	CvsDxD	TRINITY_DN1735_c0_g1	167.2630849	10.61158679	3.093625568	3.430145814	0.000603257	0.013466825	up_regulated
stress	under	CvsDxD	TRINITY_DN160_c2_g2	176.1450632	10.68619689	2.802109509	3.81362572	0.000136943	0.003849615	up_regulated
stress	under	CvsDxD	TRINITY_DN1150_c0_g1	717.9519514	11.40882857	3.338571098	3.417278902	0.000632504	0.013681185	up_regulated
stress	under	CvsDxD	TRINITY_DN10234_c0_g1	312.7201387	11.51430566	2.981570251	3.861826049	0.000112543	0.003416796	up_regulated
stress	under	CvsDxD	TRINITY_DN171_c0_g1	416.4942689	11.92768012	2.802334379	4.256337219	2.08E-05	0.000788613	up_regulated
stress	under	CvsDxD	TRINITY_DN655801_c0_g1	463.1031625	12.08073142	3.149135413	3.836205761	0.00012495	0.003647571	up_regulated
stress	under	CvsDxD	TRINITY_DN471033_c0_g1	693.1848154	12.66259283	3.061588087	4.135955743	3.53E-05	0.001166485	up_regulated
stress	under	CvsDxD	TRINITY_DN7172_c0_g1	900.3113781	13.03977408	3.109594886	4.193399641	2.75E-05	0.000993223	up_regulated
stress	under	CvsDxD	TRINITY_DN38640_c0_g1	96.86472946	22.56738187	3.429201573	6.580943518	4.67E-11	2.13E-09	up_regulated
stress	under	CvsDxD	TRINITY_DN20873_c0_g1	123.6714802	22.90711802	3.437861926	6.663187327	2.68E-11	1.69E-09	up_regulated
stress	under	CvsDxD	TRINITY_DN20579_c0_g1	190.3504567	23.30799138	3.455597638	6.744995749	1.53E-11	1.06E-09	up_regulated
stress	under	CvsDxD	TRINITY_DN900_c0_g1	182.4661183	23.43513863	3.453749782	6.78541878	1.16E-11	8.79E-10	up_regulated
stress	under	CvsDxD	TRINITY_DN660427_c0_g1	1302.717978	26.18132279	3.548740015	7.377639017	1.61E-13	4.08E-11	up_regulated
stress	under	CvsDxD	TRINITY_DN443057_c0_g1	1705.495039	26.56289151	3.562224431	7.456827054	8.86E-14	3.36E-11	up_regulated
stress	under	CvsDxD	TRINITY_DN3456_c4_g1	2261.904066	26.7457582	3.576305587	7.478599786	7.51E-14	3.36E-11	up_regulated
   ")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


HSP20_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

HSP20_reg

HSP20_reg_final <- HSP20_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

HSP20_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

HSP20_reg_final_plot <- HSP20_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                               panel.background = element_rect(fill = "white"), 
                                                                                               panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                               axis.title.x = element_text(color="black", vjust=1),
                                                                                               axis.title.y = element_text(color="black" , vjust=1)) 






HSP20_reg_final_plot


HSP20_reg_final_plot_facet <- HSP20_reg_final_plot + facet_grid( ~ location, scales='free_y') #scales='fixed' makes that all y-axes are the same

HSP20_reg_final_plot_facet

jpeg("HSP20_regulation_FACET.jpeg",height=6,width=10,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(HSP20_reg_final_plot_facet)
dev.off()





################ heat shock protein 70 


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	inter	CvsDr	TRINITY_DN8243_c0_g1	99.90989177	-23.25462779	3.587503851	-6.482119255	9.04E-11	2.98E-09	down_regulated
stress	inter	CvsDr	TRINITY_DN8629_c1_g3	47.96558963	-8.902775647	3.574519584	-2.490621589	0.012751985	0.096583235	down_regulated
stress	inter	CvsDr	TRINITY_DN5943_c0_g1	42.21856046	-8.718658554	3.574709539	-2.438983772	0.01472863	0.101663468	down_regulated
stress	inter	CvsDr	TRINITY_DN88882_c0_g1	34.70321462	-8.435858526	3.577002908	-2.358359426	0.018355911	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN13365_c0_g1	26.13128271	-8.026530768	3.586223189	-2.238157066	0.025210814	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN16803_c0_g1	20.99876044	-7.711116676	3.597587925	-2.143412986	0.032079951	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN25413_c0_g1	20.33564169	-7.664824647	3.599812503	-2.129228853	0.033235331	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN12117_c0_g1	19.07393615	-7.572439471	3.079449167	-2.459024021	0.01393153	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN38173_c0_g2	18.57423916	-7.534155577	3.227274536	-2.334525772	0.019568208	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN10478_c0_g1	18.56732502	-7.533584817	3.606840449	-2.088693671	0.036735307	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN20797_c1_g1	15.76525654	-7.297578314	3.097392374	-2.35603935	0.018470961	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN2592_c1_g1	15.47277085	-7.270560338	3.624462881	-2.005969043	0.044859551	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN28403_c1_g1	15.03069168	-7.228741806	3.627742244	-1.992628285	0.046302166	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN32949_c0_g1	14.51737928	-7.178528106	3.632490068	-1.97620034	0.048132091	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN172381_c0_g1	14.14653335	-7.14128249	3.635060917	-1.964556483	0.049465593	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN6475_c1_g1	13.04133543	-7.023930336	3.645911868	-1.92652225	0.054039196	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN59232_c0_g1	12.59925626	-6.974179395	3.650888153	-1.910269256	0.056098554	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN6475_c0_g2	12.50728061	-6.963511397	3.652606408	-1.906449975	0.05659185	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN65662_c0_g2	12.50728061	-6.963511397	3.652606408	-1.906449975	0.05659185	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN877364_c0_g1	11.93613751	-6.89618039	3.659164225	-1.884632655	0.05947945	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN14146_c0_g1	8.883068178	-6.469966119	3.215050241	-2.012399693	0.044177821	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN48529_c0_g1	8.864630473	-6.467004257	3.237696886	-1.997408801	0.045780792	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN78957_c1_g1	8.848497481	-6.464400188	3.315718737	-1.949622601	0.051221118	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN14896_c0_g1	38.03316977	-6.180127388	3.250514119	-1.901276894	0.057265754	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN47642_c0_g2	21.23621525	-5.245806282	2.56037161	-2.048845669	0.040477208	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN8243_c0_g1	117.5906749	-23.86815768	3.501051039	-6.81742637	9.27E-12	5.31E-10	down_regulated
stress	inter	CvsDi	TRINITY_DN2818_c0_g1	91.05472613	-23.51945331	3.494439325	-6.73053704	1.69E-11	7.27E-10	down_regulated
stress	inter	CvsDi	TRINITY_DN207_c4_g2	72.00873166	-9.779970933	2.92629357	-3.342101775	0.000831466	0.020405157	down_regulated
stress	inter	CvsDi	TRINITY_DN1080_c3_g1	67.94915238	-9.696456247	2.917765425	-3.323247361	0.00088976	0.020405157	down_regulated
stress	inter	CvsDi	TRINITY_DN8629_c1_g3	56.4539302	-9.428786505	3.489053535	-2.702390895	0.006884277	0.089270477	down_regulated
stress	inter	CvsDi	TRINITY_DN5943_c0_g1	49.68986483	-9.244646655	3.489573834	-2.649219387	0.008067794	0.09096593	down_regulated
stress	inter	CvsDi	TRINITY_DN88882_c0_g1	40.84454858	-8.961806841	3.492474702	-2.566033431	0.010286891	0.098296961	down_regulated
stress	inter	CvsDi	TRINITY_DN7446_c0_g1	27.40605832	-8.386031658	3.095534219	-2.709074126	0.006747126	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN15115_c0_g1	23.67422879	-8.174849214	3.518604141	-2.32332166	0.020161879	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN10478_c0_g1	21.85313427	-8.059346186	3.525348872	-2.286113085	0.022247644	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN2592_c1_g1	18.21094523	-7.79624559	3.544275961	-2.199672282	0.027830155	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN28403_c1_g1	17.6906325	-7.754413893	3.547784358	-2.185706094	0.028837115	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN172381_c0_g1	16.65000706	-7.666926099	3.555602516	-2.156294486	0.031060678	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN32949_c0_g1	15.06290805	-7.5234216	3.562880173	-2.111612301	0.034719717	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN59232_c0_g1	14.82891254	-7.499764643	3.572464997	-2.099324877	0.035788272	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN877364_c0_g1	14.04844346	-7.421736569	3.581262634	-2.072379863	0.03823003	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN58978_c0_g1	13.7882871	-7.39476034	3.584449172	-2.063011633	0.039111523	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN558662_c0_g1	13.52813074	-7.367270142	3.58777515	-2.053436973	0.040030217	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1399597_c0_g1	13.26797438	-7.339245993	3.591248907	-2.043647261	0.040988407	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN23959_c0_g1	13.26797438	-7.339245993	3.591248907	-2.043647261	0.040988407	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN24308_c1_g1	13.10622358	-7.322044049	3.084914704	-2.373499675	0.017620409	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN19441_c0_g1	13.00781802	-7.310666733	3.594879441	-2.033633353	0.041988576	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN65662_c0_g2	12.97727463	-7.308485624	3.588169314	-2.036828529	0.041667228	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN4770_c0_g1	33.64809569	-7.245471593	2.800857071	-2.586876592	0.009685027	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN6523_c2_g1	11.70703622	-7.158607521	3.615753982	-1.979838108	0.047721723	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN42452_c0_g1	11.44687986	-7.126173368	3.620560567	-1.96825139	0.04903912	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN17675_c0_g1	11.3551153	-7.115916174	3.615177762	-1.968344751	0.049028384	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN93428_c0_g1	11.3551153	-7.115916174	3.615177762	-1.968344751	0.049028384	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN24656_c0_g1	11.12337825	-7.086181154	3.619747842	-1.957644969	0.050271687	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN15549_c0_g1	10.65990416	-7.0248065	3.629538088	-1.935454686	0.052934524	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN37774_c1_g2	10.66641078	-7.024252863	3.636521922	-1.931585458	0.053410692	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN9948_c0_g1	9.94716286	-6.923576178	3.202685737	-2.161803169	0.030633351	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN37144_c0_g2	9.464034532	-6.852649243	3.171224488	-2.160884311	0.030704277	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN17071_c0_g1	9.370011513	-6.837431552	3.201504504	-2.135693248	0.032704425	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN22865_c3_g1	9.118620231	-6.799238706	3.19228442	-2.129897531	0.033180074	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN16075_c0_g1	8.736021538	-6.736604898	3.201912592	-2.103931542	0.035384419	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN65565_c0_g1	7.922750604	-6.595124766	3.282152614	-2.009390038	0.044495786	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN180311_c0_g1	7.907478909	-6.594266776	3.392523411	-1.94376456	0.051923849	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN9990_c0_g1	7.89871383	-6.591461328	3.229257506	-2.041169314	0.041234001	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN4330_c2_g1	7.776271498	-6.567987924	3.404542396	-1.929183767	0.053708053	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN11851_c1_g1	7.610138157	-6.537886518	3.241635334	-2.01684824	0.043711353	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN7923_c0_g1	20.98888743	-6.511046489	3.021631525	-2.154811543	0.031176585	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1446165_c0_g1	7.094207975	-6.437487936	3.309244944	-1.945304154	0.051738382	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN88160_c0_g1	6.919309555	-6.401282426	3.298633246	-1.940586282	0.052308479	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN6305_c6_g1	6.886507702	-6.393688397	3.280209926	-1.949170492	0.051275068	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN30437_c0_g2	6.825286536	-6.380151945	3.30911348	-1.928054744	0.053848317	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN395806_c0_g1	6.744411135	-6.364126059	3.296256094	-1.930713475	0.053518496	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN9917_c4_g1	6.536710862	-6.317933835	3.31784549	-1.904227865	0.056880514	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN841_c2_g1	14.09918427	-5.978191464	2.896709414	-2.06378708	0.039037909	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1318473_c0_g1	11.05818335	-5.621507362	2.96573262	-1.895486911	0.058027924	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN2210_c0_g1	38.60664632	-5.432816224	2.441005545	-2.225646818	0.026037848	0.203568627	down_regulated
stress	inter	CvsDi	TRINITY_DN38173_c0_g2	22.4468024	-5.00677346	2.606553009	-1.920840836	0.054751777	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN8243_c0_g1	99.88184688	-23.16095963	3.915908503	-5.91458141	3.33E-09	5.35E-08	down_regulated
stress	inter	CvsDxD	TRINITY_DN2818_c0_g1	77.34213807	-9.370041532	3.908189588	-2.39754017	0.016505571	0.082986683	down_regulated
stress	inter	CvsDxD	TRINITY_DN8629_c1_g3	47.9521256	-8.680338578	3.898544065	-2.226559052	0.025976758	0.111273576	down_regulated
stress	inter	CvsDxD	TRINITY_DN5943_c0_g1	42.20670963	-8.496199378	3.897392515	-2.179970158	0.029259674	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN6178_c0_g1	30.58148195	-8.031520293	3.308784453	-2.427332577	0.015210305	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN207_c4_g2	60.95629661	-7.557010365	3.10436041	-2.434321202	0.014919749	0.082986683	down_regulated
stress	inter	CvsDxD	TRINITY_DN16803_c0_g1	20.99286605	-7.48847273	3.907860021	-1.916259203	0.055332111	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN25413_c0_g1	20.32993344	-7.442169362	3.909246285	-1.903735099	0.056944693	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN38173_c0_g2	18.46539029	-7.303297177	3.498052273	-2.087818193	0.036814238	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN12117_c0_g1	18.10132162	-7.274820726	3.360561556	-2.164763419	0.030405808	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN841_c2_g1	11.60095929	-6.633027146	3.408361544	-1.946104326	0.051642209	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN224_c0_g2	142.6540655	5.58910087	2.805185514	1.992417558	0.046325263	0.278937219	up_regulated
stress	inter	CvsDr	TRINITY_DN229_c0_g1	47.78126838	6.137078115	2.88256092	2.129036744	0.033251221	0.213865805	up_regulated
stress	inter	CvsDr	TRINITY_DN20581_c0_g1	6.481262051	6.309210384	3.314842062	1.903321566	0.056998598	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN183557_c0_g1	6.619392778	6.339050865	3.31066009	1.914739264	0.055525764	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN196971_c0_g1	6.633327974	6.34315326	3.318822309	1.911266308	0.055970367	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN34075_c0_g1	6.85445926	6.390617396	3.317039713	1.926602618	0.054029172	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN399205_c0_g1	7.441514849	6.507137396	3.346183008	1.944644803	0.051817743	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN40231_c0_g1	7.759581891	6.569119045	3.260360046	2.014844665	0.043920927	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN1803_c5_g1	8.056440831	6.621671187	3.356621982	1.972718769	0.048527612	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN1102212_c0_g1	8.312104799	6.666830378	3.321695702	2.007056328	0.044743666	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN15382_c0_g1	8.50597631	6.702373388	3.337020948	2.008490055	0.044591242	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN20114_c0_g1	8.567768767	6.71062281	3.295016896	2.03659739	0.041690405	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN11042_c0_g2	9.438358716	6.850293172	3.276243952	2.09089838	0.036537174	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN17350_c0_g1	9.590424639	6.873604624	3.228938772	2.128750375	0.033274919	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN11209_c0_g1	9.624957321	6.878726975	3.236195722	2.125559628	0.033539944	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN79650_c0_g2	9.970894755	6.931349067	3.676552445	1.88528497	0.059391374	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN714002_c0_g1	10.22655872	6.967855101	3.671557004	1.897792978	0.057723359	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN6589_c3_g1	10.44707939	6.996883684	3.675237011	1.903791147	0.05693739	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN666_c20_g1	10.48222269	7.003459981	3.666832489	1.909948164	0.056139888	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN18630_c0_g1	10.73727604	7.036432374	3.670142343	1.917209665	0.055211301	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN85950_c0_g1	11.24860398	7.103467207	3.297213146	2.154385201	0.031209976	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN6305_c4_g1	12.0162065	7.200397303	3.643194922	1.976396393	0.0481099	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN3218_c0_g1	12.18825929	7.219385174	3.64874333	1.978594963	0.047861627	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN44546_c0_g1	12.18825929	7.219385174	3.64874333	1.978594963	0.047861627	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN820163_c0_g1	12.18825929	7.219385174	3.64874333	1.978594963	0.047861627	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN224_c3_g1	12.40272829	7.245103634	3.143656994	2.304673712	0.021184854	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN18926_c0_g5	12.87286125	7.299563374	3.162142639	2.308423183	0.020975611	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN224_c4_g1	13.13518751	7.328007186	3.129660984	2.341469963	0.019207972	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN202324_c0_g1	14.31718221	7.453049098	3.618477767	2.059719467	0.039425367	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN18354_c4_g1	14.62736573	7.482628059	3.201271818	2.337392288	0.019418796	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN4938_c1_g2	15.71908697	7.5865533	3.171558963	2.392058098	0.01675419	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN34108_c2_g1	17.12160234	7.70990927	3.606615662	2.137713023	0.032540042	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN18420_c0_g1	18.13759592	7.793347081	3.092097735	2.520407746	0.011721896	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN746995_c0_g1	18.61439117	7.830838428	3.083528967	2.539570249	0.011098875	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN21669_c2_g1	18.66346967	7.83537278	3.591159791	2.18185022	0.029120589	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN18313_c0_g2	19.15297889	7.871713928	3.596804466	2.188529847	0.02863103	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN9777_c1_g1	19.17479761	7.874353634	3.588964294	2.194046245	0.028232085	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN20849_c1_g1	20.02356884	7.935864374	3.593393892	2.208459359	0.027212268	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN17299_c0_g1	25.01931496	8.25732922	3.092638277	2.669995156	0.007585234	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN4938_c1_g1	27.27848508	8.382046629	3.576181188	2.343854013	0.01908564	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN1116925_c0_g1	31.44666807	8.587852814	3.563872846	2.40969675	0.015965785	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN102918_c0_g1	31.70233204	8.599532033	3.563662762	2.413116113	0.01581678	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN3947_c0_g1	86.82675208	8.602999819	2.901923633	2.964585188	0.003030913	0.042966542	up_regulated
stress	inter	CvsDr	TRINITY_DN3169_c0_g1	33.73248736	8.688638044	3.026826596	2.870543709	0.004097665	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN1192_c0_g1	1514.573944	14.17736886	3.299067178	4.297387139	1.73E-05	0.000444627	up_regulated
stress	inter	CvsDr	TRINITY_DN395_c0_g2	385.5412639	24.83821047	3.628362448	6.84557037	7.62E-12	5.39E-10	up_regulated
stress	inter	CvsDi	TRINITY_DN11951_c0_g1	27.77672607	5.12598658	2.695583898	1.901623831	0.05722035	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN63351_c0_g2	58.76225988	6.274202424	2.911658434	2.154855237	0.031173164	0.228161032	up_regulated
stress	inter	CvsDi	TRINITY_DN106330_c0_g1	7.063929126	6.288558387	3.293497926	1.909385865	0.056212333	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN79949_c0_g2	7.473422195	6.370619593	3.252310339	1.958798186	0.050136426	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN80224_c0_g1	7.520108885	6.380052256	3.310824485	1.927028233	0.053976112	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN40231_c0_g1	8.178309337	6.501032826	3.271930926	1.986910168	0.046932353	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN13634_c0_g1	8.318153183	6.52465938	3.213468664	2.030410146	0.042314866	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN96639_c0_g1	10.11145946	6.80706225	3.20796807	2.121923318	0.033844178	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN68663_c1_g1	10.65032546	6.881133937	3.148273718	2.185684777	0.028838675	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN8580_c0_g2	10.72799246	6.891369621	3.639907807	1.893281365	0.058320462	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN936997_c0_g1	11.89953028	7.041886722	3.611426314	1.949890738	0.051189143	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN1380989_c0_g1	12.04962882	7.05920706	3.122726817	2.260590655	0.023784617	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN818246_c0_g1	12.11202189	7.067414544	3.607811021	1.958920382	0.050122112	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN15382_c0_g1	12.96198834	7.165234615	3.594657352	1.993301145	0.046228481	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN20121_c0_g3	13.06561642	7.176792423	3.173965986	2.261143458	0.023750375	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN131890_c0_g2	68.51773136	7.177157238	2.740000156	2.619400303	0.008808452	0.094690855	up_regulated
stress	inter	CvsDi	TRINITY_DN17179_c3_g1	13.18997011	7.190349116	3.119486131	2.30497871	0.021167766	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN14628_c3_g1	13.34006865	7.205870567	3.136278681	2.297586184	0.021585353	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN1803_c5_g1	14.06044595	7.28174256	3.13700515	2.321240232	0.020273882	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN6475_c4_g1	14.22625087	7.298655568	3.58404972	2.036426986	0.041707498	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN43977_c1_g1	15.7243793	7.443879528	3.562805256	2.089331017	0.036677936	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN185946_c0_g1	15.96261576	7.465266593	3.0420004	2.454064961	0.014125146	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN59905_c0_g1	16.99932897	7.556328168	3.552082384	2.127295302	0.033395555	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN8023_c0_g3	19.19122495	7.73062249	3.101508007	2.492536686	0.012683423	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN307098_c0_g1	23.79906056	8.041658236	3.517529367	2.286166624	0.022244513	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN1101_c2_g2	67.6199974	8.056286498	3.29423244	2.445573178	0.01446221	0.124375003	up_regulated
stress	inter	CvsDi	TRINITY_DN28127_c0_g1	24.23910149	8.067585826	3.057675151	2.638470546	0.008328093	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN1192666_c0_g1	26.77394313	8.211554844	3.509253263	2.339972133	0.019285179	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN22996_c0_g1	27.47861404	8.24898057	2.978798017	2.769231255	0.005618874	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN6615_c0_g1	29.53633408	8.353194236	3.503621736	2.384159839	0.017118165	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN14659_c1_g1	31.25110846	8.434208599	3.506516893	2.405295299	0.016159402	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN17179_c0_g2	36.97354051	8.677152906	3.494507707	2.483083065	0.013025071	0.114887807	up_regulated
stress	inter	CvsDi	TRINITY_DN2449_c3_g1	59.710143	9.368565016	3.488632595	2.685454762	0.007243118	0.089270477	up_regulated
stress	inter	CvsDi	TRINITY_DN5785_c0_g1	110.2831467	21.47742964	3.498804541	6.138505134	8.33E-10	2.20E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN7876_c0_g5	87.53804376	5.038104692	2.658662242	1.894977336	0.058095404	0.213761296	up_regulated
stress	inter	CvsDxD	TRINITY_DN1357_c1_g3	157.7348168	5.676147821	2.767009804	2.051365273	0.040231388	0.154282645	up_regulated
stress	inter	CvsDxD	TRINITY_DN9949_c4_g1	143.2273136	6.01829593	2.738605045	2.197577172	0.027979251	0.116377466	up_regulated
stress	inter	CvsDxD	TRINITY_DN1357_c0_g2	42.37997692	6.055449006	2.867697193	2.111606839	0.034720185	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN41803_c0_g1	7.282562446	6.598904422	3.505368143	1.882513948	0.059766268	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN892749_c0_g1	7.714716946	6.681705528	3.506267873	1.905646051	0.056696143	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN47272_c0_g1	8.041295643	6.741515667	3.498650717	1.926890168	0.053993319	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN28127_c0_g1	8.121530797	6.756511226	3.511720575	1.923988849	0.054355981	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN224_c4_g1	8.385470306	6.801931873	3.499050738	1.943936336	0.051903129	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN11651_c0_g2	8.897859961	6.88782662	3.458092428	1.991799457	0.046393066	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN992_c5_g1	9.347610429	6.958678557	3.473613759	2.003296578	0.045145465	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN8023_c0_g3	10.34494249	7.104908333	3.46833038	2.048509673	0.040510085	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN271436_c0_g1	11.92504807	7.310061659	3.429364412	2.131608304	0.03303906	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN8596_c0_g1	247.4606013	7.317205265	3.541643337	2.066048037	0.038823944	0.152636602	up_regulated
stress	inter	CvsDxD	TRINITY_DN841_c6_g2	12.50007691	7.378498643	3.428014567	2.152411695	0.031364943	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN307098_c0_g1	12.6682919	7.397813	3.913937226	1.890120503	0.058741846	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN44314_c0_g1	12.73656917	7.40494191	3.91659018	1.890660388	0.058669693	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN42610_c0_g1	12.99276399	7.433685011	3.470683625	2.141850372	0.032205525	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN34962_c1_g1	13.28625736	7.466511344	3.909091597	1.91003745	0.056128392	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN44854_c0_g2	13.90422282	7.532086399	3.904784954	1.928937569	0.053738613	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN20581_c0_g1	14.04288395	7.545830329	3.907099738	1.93131244	0.053444426	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN36117_c0_g1	14.04288395	7.545830329	3.907099738	1.93131244	0.053444426	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1149787_c0_g1	172.0111471	7.60539502	3.525347008	2.157346498	0.030978677	0.127012574	up_regulated
stress	inter	CvsDxD	TRINITY_DN983471_c0_g1	14.64325344	7.606226358	3.481976086	2.184456805	0.028928698	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1192666_c0_g1	15.44913646	7.684060541	3.895928655	1.972330918	0.048571842	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN30145_c0_g1	16.00235613	7.734306381	3.89651079	1.984931339	0.047152111	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN22875_c0_g1	16.63791756	7.790482002	3.554908889	2.191471637	0.02841768	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN7446_c2_g1	16.68506738	7.795072115	3.890383597	2.003676995	0.045104672	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN32146_c0_g1	16.98209222	7.820049255	3.892422051	2.009044536	0.044532411	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN3831_c2_g1	403.3892332	7.832946415	3.183035862	2.460841397	0.013861163	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN224_c3_g1	17.60990896	7.87259787	3.358736514	2.343916481	0.019082444	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN45663_c0_g1	17.61201557	7.873061508	3.886920616	2.025526705	0.042813309	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN148007_c1_g1	17.96182831	7.900980588	3.888948925	2.031649359	0.042189166	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN942006_c0_g1	18.55655972	7.948484284	3.546775833	2.241045011	0.02502316	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN61141_c0_g1	18.9415644	7.97761216	3.885987764	2.052917468	0.040080583	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN10259_c2_g1	19.92130049	8.050377888	3.883455985	2.07299321	0.038172911	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN164360_c0_g1	20.0838774	8.06250973	3.879875755	2.078032968	0.037706323	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN209912_c0_g1	21.31980832	8.148653761	3.877264205	2.101650372	0.035583914	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN17179_c0_g2	22.24675651	8.210045739	3.875612181	2.118386814	0.034142322	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN59130_c0_g1	23.82264888	8.308418099	3.543429074	2.344739495	0.019040378	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN398_c1_g1	66.46661602	8.370591784	3.34904244	2.499398539	0.012440432	0.079342309	up_regulated
stress	inter	CvsDxD	TRINITY_DN28414_c0_g4	27.10603181	8.494726863	3.873185145	2.193214769	0.028291909	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN31840_c1_g2	27.43261051	8.512006382	3.872945199	2.197812245	0.027962489	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN3169_c0_g1	27.7064013	8.526332981	3.460771995	2.463708384	0.013750794	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN15329_c0_g3	28.56296562	8.570288971	3.380521282	2.535197461	0.011238396	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1079020_c0_g1	29.39208269	8.611550479	3.87176674	2.22419145	0.026135566	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN3102_c0_g1	29.66234201	8.62503246	3.868469686	2.229572198	0.025775857	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN90977_c0_g1	30.04524008	8.643261981	3.871461809	2.232557728	0.025578124	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN15455_c3_g1	34.61734183	8.847635244	3.870252663	2.286061406	0.022250666	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN20938_c0_g1	35.59707792	8.88790176	3.870159144	2.296521003	0.02164611	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN533526_c0_g1	35.92365662	8.901077995	3.870138273	2.299937978	0.021451734	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1098601_c0_g1	42.12865185	9.130958974	3.870496846	2.359118051	0.018318427	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN28537_c0_g1	43.76154534	9.185823563	3.870770751	2.373125187	0.017638285	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN988439_c0_g1	45.06786012	9.228260895	3.871028621	2.383929905	0.017128865	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN65253_c1_g1	50.05520214	9.379852401	3.869143589	2.424270949	0.015339156	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN3738_c1_g1	52.8360467	9.457849343	3.869958359	2.443915015	0.014528849	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN666_c1_g1	53.1971356	9.467532033	3.498386878	2.706256444	0.006804648	0.054248163	up_regulated
stress	inter	CvsDxD	TRINITY_DN666_c0_g1	1800.180277	9.671407969	3.640375381	2.656706234	0.007890817	0.059596434	up_regulated
stress	inter	CvsDxD	TRINITY_DN11297_c0_g1	63.33867116	9.719279819	3.553605112	2.735047793	0.006237122	0.0511444	up_regulated
stress	inter	CvsDxD	TRINITY_DN367032_c0_g1	64.00942452	9.734465252	3.876791114	2.510959442	0.012040353	0.078535939	up_regulated
stress	inter	CvsDxD	TRINITY_DN13651_c1_g1	75.67827776	9.976087667	3.447237246	2.893937073	0.003804444	0.036296737	up_regulated
stress	inter	CvsDxD	TRINITY_DN11209_c0_g1	94.03706864	10.2894403	3.56716811	2.88448427	0.003920553	0.036296737	up_regulated
stress	inter	CvsDxD	TRINITY_DN1873_c0_g1	858.8839821	11.95212003	3.734107729	3.20079679	0.001370481	0.014567711	up_regulated
stress	inter	CvsDxD	TRINITY_DN35341_c0_g1	81.64467414	22.79291067	3.883136163	5.869717082	4.37E-09	5.57E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN20125_c0_g1	97.94752517	23.04241921	3.88579497	5.929911224	3.03E-09	5.35E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN36193_c0_g1	121.1212299	23.17432863	3.893275901	5.952398243	2.64E-09	5.20E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN1237_c5_g1	271.595819	24.13531773	3.926897576	6.146154123	7.94E-10	2.85E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN12642_c2_g1	258.6185444	24.28300702	3.924702412	6.187222488	6.12E-10	2.85E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN19942_c0_g1	245.6412698	24.32671593	3.922409828	6.201982199	5.58E-10	2.85E-08	up_regulated
stress	under	CvsDi	TRINITY_DN6578_c0_g1	295.3610632	-25.08906816	3.281576016	-7.645432573	2.08E-14	6.75E-12	down_regulated
stress	under	CvsDi	TRINITY_DN1426_c0_g1	196.1155228	-24.50497665	3.263376263	-7.509087114	5.95E-14	8.52E-12	down_regulated
stress	under	CvsDi	TRINITY_DN1044_c3_g1	154.4112797	-24.36103179	3.253445466	-7.487763986	7.01E-14	8.52E-12	down_regulated
stress	under	CvsDi	TRINITY_DN1795_c4_g1	85.52009338	-23.55787499	3.23423656	-7.283905972	3.24E-13	2.63E-11	down_regulated
stress	under	CvsDi	TRINITY_DN1805_c0_g1	1065.657936	-10.75932991	2.175250649	-4.946248338	7.57E-07	4.33E-05	down_regulated
stress	under	CvsDi	TRINITY_DN275587_c0_g1	63.61216822	-9.728653339	3.228827943	-3.013060315	0.002586275	0.068643986	down_regulated
stress	under	CvsDi	TRINITY_DN280_c1_g1	61.36763843	-9.67631388	2.683950367	-3.605250678	0.000311852	0.015970093	down_regulated
stress	under	CvsDi	TRINITY_DN6765_c0_g1	53.054132	-9.466902919	3.227655178	-2.933058954	0.003356402	0.077756635	down_regulated
stress	under	CvsDi	TRINITY_DN2270_c2_g1	46.41820171	-9.273310454	2.712917913	-3.418205325	0.000630355	0.027878898	down_regulated
stress	under	CvsDi	TRINITY_DN36572_c0_g1	24.76898161	-8.368786305	2.913021853	-2.872888267	0.004067379	0.084203407	down_regulated
stress	under	CvsDi	TRINITY_DN226469_c0_g1	20.58817063	-8.102021609	3.263177458	-2.482862704	0.013033131	0.144015439	down_regulated
stress	under	CvsDi	TRINITY_DN106737_c0_g1	20.13411795	-8.070129778	3.010524464	-2.680639162	0.00734817	0.112025547	down_regulated
stress	under	CvsDi	TRINITY_DN1109470_c0_g1	18.47656338	-7.946034937	3.273884916	-2.427096596	0.015220203	0.151114871	down_regulated
stress	under	CvsDi	TRINITY_DN19203_c0_g1	12.22512066	-7.348692277	2.875822404	-2.555335916	0.010608536	NA	down_regulated
stress	under	CvsDi	TRINITY_DN550370_c0_g1	11.08593803	-7.209856737	3.353919263	-2.149681066	0.031580451	NA	down_regulated
stress	under	CvsDi	TRINITY_DN3496_c0_g1	8.95241503	-6.899131034	2.960322829	-2.330533334	0.01977798	NA	down_regulated
stress	under	CvsDi	TRINITY_DN88874_c0_g1	8.297302701	-6.786630044	3.10383374	-2.186531436	0.028776747	NA	down_regulated
stress	under	CvsDi	TRINITY_DN26615_c1_g2	7.918527165	-6.725105306	3.441846066	-1.953923905	0.050710217	NA	down_regulated
stress	under	CvsDi	TRINITY_DN109233_c0_g1	7.928057035	-6.721940255	3.028522172	-2.219544673	0.026449692	NA	down_regulated
stress	under	CvsDi	TRINITY_DN40491_c4_g1	7.76940089	-6.691977791	3.463946586	-1.931894048	0.053372585	NA	down_regulated
stress	under	CvsDi	TRINITY_DN976240_c0_g1	7.390625354	-6.625721743	3.464289329	-1.912577477	0.055802165	NA	down_regulated
stress	under	CvsDi	TRINITY_DN18694_c1_g1	7.210053452	-6.583938662	3.112884961	-2.115060063	0.034424828	NA	down_regulated
stress	under	CvsDi	TRINITY_DN115315_c2_g2	6.503007747	-6.436578635	3.085324858	-2.086191546	0.036961276	NA	down_regulated
stress	under	CvsDi	TRINITY_DN7687_c1_g2	6.196653336	-6.365213545	3.162146078	-2.012941018	0.044120835	NA	down_regulated
stress	under	CvsDi	TRINITY_DN60857_c0_g1	16.4015688	-6.3000133	2.708755657	-2.325796084	0.020029432	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1554397_c0_g1	5.795962042	-6.273010345	3.127042472	-2.006052173	0.044850682	NA	down_regulated
stress	under	CvsDi	TRINITY_DN19871_c1_g1	5.690667282	-6.249216814	3.206770417	-1.948757161	0.051324432	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1323639_c0_g1	14.45345814	-6.136110639	3.126164107	-1.962824225	0.049666602	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1164312_c0_g1	22.04419273	-5.752509722	2.948879539	-1.950744222	0.05108748	0.292598074	down_regulated
stress	under	CvsDi	TRINITY_DN59636_c0_g1	9.49786976	-5.498880175	2.79030782	-1.970707366	0.048757358	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1749_c3_g1	16.48201297	-5.278292852	2.693544481	-1.959608571	0.050041559	NA	down_regulated
stress	under	CvsDi	TRINITY_DN68707_c0_g1	17.20803961	-4.739635804	2.467397952	-1.92090449	0.05474375	0.304375248	down_regulated
stress	under	CvsDi	TRINITY_DN44626_c0_g1	29.87483184	-4.549387556	2.110611811	-2.155482847	0.031124069	0.208004966	down_regulated
stress	under	CvsDr	TRINITY_DN1044_c8_g1	86.24769838	-10.26989645	2.940142039	-3.492993303	0.000477638	0.019556643	down_regulated
stress	under	CvsDr	TRINITY_DN3050_c0_g1	76.28084928	-10.09275136	2.95268052	-3.418165727	0.000630447	0.021156092	down_regulated
stress	under	CvsDr	TRINITY_DN6765_c0_g1	60.39401409	-9.755826675	3.487118019	-2.797676082	0.00514717	0.076888617	down_regulated
stress	under	CvsDr	TRINITY_DN61496_c0_g1	38.05370323	-9.089172374	2.98766434	-3.042233444	0.002348297	0.045544602	down_regulated
stress	under	CvsDr	TRINITY_DN36572_c0_g1	28.02470252	-8.648383567	3.123990896	-2.768376687	0.00563363	0.076888617	down_regulated
stress	under	CvsDr	TRINITY_DN45253_c0_g1	26.14069267	-8.547912477	3.490449457	-2.448943204	0.014327604	0.113781382	down_regulated
stress	under	CvsDr	TRINITY_DN226469_c0_g1	23.43648308	-8.39040793	3.494698999	-2.400895738	0.016354996	NA	down_regulated
stress	under	CvsDr	TRINITY_DN220299_c0_g1	23.21449712	-8.376671315	3.018730953	-2.774898275	0.005521898	NA	down_regulated
stress	under	CvsDr	TRINITY_DN74260_c0_g1	18.68313502	-8.063582907	3.16607841	-2.546867722	0.010869463	NA	down_regulated
stress	under	CvsDr	TRINITY_DN53762_c0_g1	114.639624	-7.957382441	2.612429612	-3.045970082	0.002319309	0.045544602	down_regulated
stress	under	CvsDr	TRINITY_DN6578_c0_g1	337.6825354	-7.866943449	3.204259134	-2.455152071	0.0140825	0.113781382	down_regulated
stress	under	CvsDr	TRINITY_DN1323639_c0_g1	16.22525752	-7.86004349	3.518835788	-2.233705681	0.025502445	NA	down_regulated
stress	under	CvsDr	TRINITY_DN29487_c0_g1	15.35092214	-7.78026696	3.232241966	-2.407080609	0.01608062	NA	down_regulated
stress	under	CvsDr	TRINITY_DN86373_c0_g1	14.57669297	-7.70446107	3.108403265	-2.478591229	0.013190237	NA	down_regulated
stress	under	CvsDr	TRINITY_DN861150_c0_g1	14.1219834	-7.659813798	3.532744864	-2.168232944	0.030140967	NA	down_regulated
stress	under	CvsDr	TRINITY_DN70676_c0_g1	42.38715481	-7.634847234	2.827784286	-2.699939763	0.006935203	0.086631267	down_regulated
stress	under	CvsDr	TRINITY_DN1893879_c0_g1	13.2205802	-7.564692105	3.540449781	-2.136647198	0.032626698	NA	down_regulated
stress	under	CvsDr	TRINITY_DN79172_c0_g1	12.64399086	-7.500132041	3.075763116	-2.438462182	0.014749902	NA	down_regulated
stress	under	CvsDr	TRINITY_DN550370_c0_g1	12.61964474	-7.49760398	3.546342795	-2.114179145	0.034499969	NA	down_regulated
stress	under	CvsDr	TRINITY_DN3673_c2_g1	12.31645531	-7.462306043	3.083625527	-2.419978035	0.015521445	NA	down_regulated
stress	under	CvsDr	TRINITY_DN14855_c0_g1	12.26231967	-7.456039919	3.094846394	-2.409179316	0.01598844	NA	down_regulated
stress	under	CvsDr	TRINITY_DN36619_c1_g1	12.15132669	-7.442591931	3.071620345	-2.423018177	0.015392156	NA	down_regulated
stress	under	CvsDr	TRINITY_DN37283_c1_g1	11.71824154	-7.390731985	3.55656	-2.07805632	0.037704172	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1256304_c0_g1	11.41505211	-7.352638903	3.094231229	-2.376240933	0.017490038	NA	down_regulated
stress	under	CvsDr	TRINITY_DN58476_c0_g1	10.87097399	-7.28262048	3.212636736	-2.266867087	0.023398346	NA	down_regulated
stress	under	CvsDr	TRINITY_DN12192_c0_g1	10.18339167	-7.187208816	3.116512974	-2.306170029	0.021101134	NA	down_regulated
stress	under	CvsDr	TRINITY_DN45274_c0_g1	9.991195226	-7.159426409	3.153186714	-2.27053678	0.023175034	NA	down_regulated
stress	under	CvsDr	TRINITY_DN20197_c0_g1	9.723238704	-7.121614923	3.178916765	-2.240264672	0.025073745	NA	down_regulated
stress	under	CvsDr	TRINITY_DN566137_c0_g1	9.723238704	-7.121614923	3.178916765	-2.240264672	0.025073745	NA	down_regulated
stress	under	CvsDr	TRINITY_DN8395_c6_g1	9.614967418	-7.105457238	3.589330097	-1.979605399	0.047747886	NA	down_regulated
stress	under	CvsDr	TRINITY_DN5298_c1_g1	63.05670777	-7.087667336	2.653354528	-2.671210071	0.007557833	0.092151384	down_regulated
stress	under	CvsDr	TRINITY_DN62418_c0_g1	9.395703151	-7.072251675	3.204973377	-2.206649118	0.027338583	NA	down_regulated
stress	under	CvsDr	TRINITY_DN226036_c0_g1	9.227852833	-7.045030361	3.138884209	-2.244437798	0.024804247	NA	down_regulated
stress	under	CvsDr	TRINITY_DN2270_c8_g1	9.203506706	-7.042168434	3.153179062	-2.233355067	0.025525539	NA	down_regulated
stress	under	CvsDr	TRINITY_DN26615_c1_g2	9.014031954	-7.012393425	3.601931069	-1.94684276	0.051553589	NA	down_regulated
stress	under	CvsDr	TRINITY_DN175469_c0_g1	8.927385101	-6.997211323	3.150125191	-2.221248648	0.026334125	NA	down_regulated
stress	under	CvsDr	TRINITY_DN209240_c0_g1	8.220900045	-6.879533267	3.208846299	-2.143927326	0.03203871	NA	down_regulated
stress	under	CvsDr	TRINITY_DN9408_c0_g1	8.220900045	-6.879533267	3.208846299	-2.143927326	0.03203871	NA	down_regulated
stress	under	CvsDr	TRINITY_DN88874_c0_g1	8.134253192	-6.862448479	3.264951467	-2.101853136	0.035566143	NA	down_regulated
stress	under	CvsDr	TRINITY_DN26681_c1_g1	8.001635778	-6.840256221	3.181598412	-2.14994331	0.031559699	NA	down_regulated
stress	under	CvsDr	TRINITY_DN109233_c0_g1	7.998914084	-6.838640146	3.185813945	-2.14659119	0.031825842	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1164312_c0_g1	24.8815446	-6.825367974	3.265514081	-2.090135827	0.0366056	0.20426554	down_regulated
stress	under	CvsDr	TRINITY_DN1617193_c0_g1	7.86629667	-6.81609958	3.266646744	-2.086573821	0.036926676	NA	down_regulated
stress	under	CvsDr	TRINITY_DN72868_c1_g1	7.674100225	-6.780051981	3.198974332	-2.119445571	0.034052829	NA	down_regulated
stress	under	CvsDr	TRINITY_DN18374_c0_g1	7.538761117	-6.754830263	3.328760022	-2.029233173	0.042434546	NA	down_regulated
stress	under	CvsDr	TRINITY_DN16478_c0_g6	7.343842977	-6.715392216	3.205631317	-2.094873537	0.036182236	NA	down_regulated
stress	under	CvsDr	TRINITY_DN65048_c0_g1	7.073164761	-6.66239359	3.21861538	-2.069956426	0.038456425	NA	down_regulated
stress	under	CvsDr	TRINITY_DN160073_c0_g1	22.17733501	-6.657459653	3.269203834	-2.036416201	0.04170858	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1878999_c0_g1	6.496575425	-6.537971688	3.315906354	-1.971699738	0.048643894	NA	down_regulated
stress	under	CvsDr	TRINITY_DN398341_c0_g1	6.418093654	-6.522433666	3.266641736	-1.996678605	0.045860108	NA	down_regulated
stress	under	CvsDr	TRINITY_DN9244_c4_g1	6.418093654	-6.522433666	3.266641736	-1.996678605	0.045860108	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1812170_c0_g1	6.391025833	-6.516471712	3.286056689	-1.983067344	0.047359908	NA	down_regulated
stress	under	CvsDr	TRINITY_DN19871_c1_g1	6.363958011	-6.510472873	3.315911027	-1.96340397	0.049599253	NA	down_regulated
stress	under	CvsDr	TRINITY_DN6027_c0_g1	6.33689019	-6.504422725	3.369014777	-1.930660194	0.053525089	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1841662_c1_g1	6.14197205	-6.457080271	3.301960485	-1.955529238	0.050520636	NA	down_regulated
stress	under	CvsDr	TRINITY_DN238626_c0_g1	5.898361656	-6.400096728	3.264433889	-1.960553329	0.049931151	NA	down_regulated
stress	under	CvsDr	TRINITY_DN67013_c0_g2	5.70616521	-6.351524597	3.277959207	-1.937646015	0.052666424	NA	down_regulated
stress	under	CvsDr	TRINITY_DN124705_c0_g1	5.652029567	-6.338128115	3.275913897	-1.934766393	0.053018969	NA	down_regulated
stress	under	CvsDr	TRINITY_DN162757_c0_g2	5.624961745	-6.331385542	3.277964878	-1.931498896	0.053421386	NA	down_regulated
stress	under	CvsDr	TRINITY_DN37231_c0_g2	25.58450237	-6.312620118	2.687948574	-2.348489915	0.01884971	0.138031639	down_regulated
stress	under	CvsDr	TRINITY_DN275587_c0_g1	73.38548571	-6.247420996	3.1401226	-1.98954684	0.046640877	0.236241366	down_regulated
stress	under	CvsDr	TRINITY_DN137524_c0_g1	5.297426192	-6.244951877	3.302031012	-1.891245677	0.058591557	NA	down_regulated
stress	under	CvsDr	TRINITY_DN18209_c0_g1	612.9440024	-5.661179285	2.202590865	-2.570236432	0.010162913	0.099501893	down_regulated
stress	under	CvsDr	TRINITY_DN98015_c0_g1	10.67563604	-5.613769336	2.916996423	-1.924503332	0.054291523	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1380_c2_g1	22.21107507	-5.538532023	2.663949193	-2.079068189	0.037611083	NA	down_regulated
stress	under	CvsDr	TRINITY_DN59636_c0_g1	9.963707597	-5.512166414	2.906901671	-1.896234217	0.05792908	NA	down_regulated
stress	under	CvsDr	TRINITY_DN14245_c0_g2	29.05040915	-5.055037646	2.442765302	-2.069391456	0.038509368	0.210232622	down_regulated
stress	under	CvsDr	TRINITY_DN10446_c0_g2	31.20701333	-5.050193577	2.657796096	-1.90014335	0.05741431	0.263545208	down_regulated
stress	under	CvsDxD	TRINITY_DN118452_c1_g1	47.10271251	-9.266031538	3.379856205	-2.741546083	0.006115078	0.064459963	down_regulated
stress	under	CvsDxD	TRINITY_DN45557_c0_g1	23.67729934	-8.274292673	3.389607073	-2.441077238	0.014643522	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN106737_c0_g1	19.2266988	-7.974425184	3.127426403	-2.549836241	0.010777353	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN160073_c0_g1	18.38768991	-7.909838946	3.403880765	-2.323770864	0.020137778	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN86373_c0_g1	14.58554297	-7.572331319	3.02834636	-2.500483901	0.012402377	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1323639_c0_g1	13.60185281	-7.475366957	3.431993409	-2.178141408	0.029395511	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN114717_c0_g1	13.09808048	-7.42098579	3.436532129	-2.159440247	0.030816028	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN861150_c0_g1	11.83864967	-7.27532235	3.449966333	-2.108809666	0.034961014	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN89013_c0_g1	10.07544653	-7.042990359	3.475608754	-2.026404828	0.042723317	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN16478_c0_g6	7.126988162	-6.538794521	3.12901009	-2.089732642	0.036641823	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1878999_c0_g1	6.538030474	-6.412798526	3.233894416	-1.98299564	0.047367917	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN65048_c0_g1	6.293484156	-6.36343808	3.156428179	-2.016024987	0.043797363	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1841662_c1_g1	6.11944351	-6.317502024	3.222219061	-1.960606	0.049925002	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN978384_c0_g1	13.8754722	-6.067914179	2.877770647	-2.108546831	0.034983716	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN14889_c0_g1	31.80469119	-5.738435337	2.60790374	-2.200401514	0.027778421	0.139462498	down_regulated
stress	under	CvsDxD	TRINITY_DN30677_c0_g2	38.41926354	-4.370840225	2.260221981	-1.933810158	0.053136473	0.229151042	down_regulated
stress	under	CvsDi	TRINITY_DN280_c15_g1	119.3637195	4.476687798	2.163970861	2.06873756	0.038570721	0.239040198	up_regulated
stress	under	CvsDi	TRINITY_DN786_c0_g2	64.49368631	4.691135936	2.262510631	2.073420505	0.038133162	0.238177929	up_regulated
stress	under	CvsDi	TRINITY_DN283508_c0_g1	19.76433358	5.136069339	2.579678797	1.990972421	0.046483919	0.277477629	up_regulated
stress	under	CvsDi	TRINITY_DN1380_c1_g3	52.2091289	5.257875092	2.576760388	2.040498261	0.041300724	0.252739651	up_regulated
stress	under	CvsDi	TRINITY_DN5756_c0_g5	27.29767444	5.341115484	2.7329075	1.954371117	0.050657344	0.292598074	up_regulated
stress	under	CvsDi	TRINITY_DN38134_c1_g1	28.90769919	5.562623418	2.396212762	2.321423	0.020264025	0.164412176	up_regulated
stress	under	CvsDi	TRINITY_DN5940_c0_g2	71.14012467	5.71652546	2.461982527	2.32191959	0.020237266	0.164412176	up_regulated
stress	under	CvsDi	TRINITY_DN36958_c1_g1	46.66968682	5.819190966	2.542332532	2.288918107	0.022084111	0.173152754	up_regulated
stress	under	CvsDi	TRINITY_DN1810_c1_g3	40.81062983	5.939762787	2.521462872	2.355681241	0.018488775	0.162153655	up_regulated
stress	under	CvsDi	TRINITY_DN3223_c0_g2	6.784695748	5.948407898	3.072514076	1.93600672	0.052866878	NA	up_regulated
stress	under	CvsDi	TRINITY_DN16143_c1_g2	6.807332102	5.953138458	3.095653491	1.923063571	0.054472067	NA	up_regulated
stress	under	CvsDi	TRINITY_DN54996_c0_g1	7.162277874	6.026658497	3.034880906	1.985797362	0.047055829	NA	up_regulated
stress	under	CvsDi	TRINITY_DN53354_c1_g1	7.184914228	6.031137606	3.043361164	1.981735746	0.047508824	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1390809_c0_g1	7.219023539	6.038484633	3.153463144	1.914874015	0.055508572	NA	up_regulated
stress	under	CvsDi	TRINITY_DN2270_c0_g2	134.4062495	6.09893509	2.280388355	2.674515977	0.007483721	0.112025547	up_regulated
stress	under	CvsDi	TRINITY_DN8655_c1_g1	7.577587256	6.107964385	3.016304262	2.024982845	0.042869126	NA	up_regulated
stress	under	CvsDi	TRINITY_DN80153_c0_g1	7.811805808	6.152321543	3.18693402	1.930482873	0.053547035	NA	up_regulated
stress	under	CvsDi	TRINITY_DN29671_c0_g3	8.219569739	6.225714679	3.119403664	1.995802836	0.045955388	NA	up_regulated
stress	under	CvsDi	TRINITY_DN124331_c0_g2	8.257296995	6.232252887	3.027522715	2.058532164	0.039539077	NA	up_regulated
stress	under	CvsDi	TRINITY_DN20729_c0_g1	23.99565349	6.241526733	2.607865399	2.393346963	0.016695445	0.157715219	up_regulated
stress	under	CvsDi	TRINITY_DN100449_c0_g1	8.42722443	6.261695452	3.082901623	2.03110453	0.042244392	NA	up_regulated
stress	under	CvsDi	TRINITY_DN155697_c0_g1	8.578133456	6.286955846	2.969776744	2.116979285	0.034261606	NA	up_regulated
stress	under	CvsDi	TRINITY_DN26319_c0_g1	8.66113342	6.300655663	3.009022612	2.093921008	0.036267017	NA	up_regulated
stress	under	CvsDi	TRINITY_DN513972_c0_g1	9.020006699	6.359759887	3.097576457	2.053140568	0.040058946	NA	up_regulated
stress	under	CvsDi	TRINITY_DN143099_c0_g1	9.220115939	6.391411506	3.092767053	2.06656738	0.038774937	NA	up_regulated
stress	under	CvsDi	TRINITY_DN39369_c0_g1	9.605243516	6.450380361	3.416236512	1.88815392	0.05900529	NA	up_regulated
stress	under	CvsDi	TRINITY_DN71792_c0_g1	9.605243516	6.450380361	3.416236512	1.88815392	0.05900529	NA	up_regulated
stress	under	CvsDi	TRINITY_DN793182_c0_g1	9.605243516	6.450380361	3.416236512	1.88815392	0.05900529	NA	up_regulated
stress	under	CvsDi	TRINITY_DN96374_c0_g1	9.605243516	6.450380361	3.416236512	1.88815392	0.05900529	NA	up_regulated
stress	under	CvsDi	TRINITY_DN56194_c1_g2	9.612788967	6.451570642	3.138529911	2.055602726	0.039820825	NA	up_regulated
stress	under	CvsDi	TRINITY_DN4778_c2_g1	9.759770486	6.472919029	3.414680488	1.895614846	0.058010993	NA	up_regulated
stress	under	CvsDi	TRINITY_DN112154_c0_g1	9.907061567	6.494564667	2.978462538	2.180509099	0.029219745	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1152274_c0_g1	10.005462	6.509265734	3.406290242	1.91095452	0.056010426	NA	up_regulated
stress	under	CvsDi	TRINITY_DN57690_c0_g1	10.40568048	6.565841502	3.397166571	1.932740525	0.05326817	NA	up_regulated
stress	under	CvsDi	TRINITY_DN71908_c0_g1	10.41322593	6.566942628	3.122847409	2.102870159	0.035477121	NA	up_regulated
stress	under	CvsDi	TRINITY_DN196149_c3_g3	10.59038925	6.590770947	3.395599114	1.940974398	0.052261383	NA	up_regulated
stress	under	CvsDi	TRINITY_DN17284_c1_g1	10.60578972	6.593318529	3.392884269	1.943278345	0.051982536	NA	up_regulated
stress	under	CvsDi	TRINITY_DN16406_c1_g2	10.62088062	6.595428035	3.064709378	2.152056597	0.031392897	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1312504_c0_g1	32.55538766	6.666571483	3.08457242	2.161262754	0.030675048	0.207269597	up_regulated
stress	under	CvsDi	TRINITY_DN22872_c0_g2	11.24384469	6.677634689	2.985502547	2.236686984	0.025306805	NA	up_regulated
stress	under	CvsDi	TRINITY_DN73258_c1_g1	11.40622668	6.698274266	3.377390238	1.983269268	0.04733736	NA	up_regulated
stress	under	CvsDi	TRINITY_DN60930_c0_g1	11.42100802	6.699718854	3.379520108	1.982446809	0.047429255	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1599398_c0_g1	11.60633591	6.723362039	3.373883203	1.992766683	0.046287002	NA	up_regulated
stress	under	CvsDi	TRINITY_DN542189_c0_g1	13.28990024	6.91838403	3.351398715	2.06432735	0.038986689	NA	up_regulated
stress	under	CvsDi	TRINITY_DN147385_c0_g2	35.93621377	6.939223421	2.73159412	2.540356698	0.011073946	0.132698631	up_regulated
stress	under	CvsDi	TRINITY_DN105060_c0_g1	13.60742831	6.952815973	3.345086913	2.078515792	0.037661878	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1426_c2_g1	13.80753755	6.973875158	3.342724369	2.086284835	0.03695283	NA	up_regulated
stress	under	CvsDi	TRINITY_DN35219_c0_g1	13.80753755	6.973875158	3.342724369	2.086284835	0.03695283	NA	up_regulated
stress	under	CvsDi	TRINITY_DN40491_c0_g1	40.60017378	6.999958751	2.79274671	2.506478202	0.012194058	0.136377219	up_regulated
stress	under	CvsDi	TRINITY_DN21593_c0_g1	14.40786527	7.035268874	3.336089379	2.108837046	0.03495865	NA	up_regulated
stress	under	CvsDi	TRINITY_DN127987_c0_g1	14.43804708	7.038339302	2.966196438	2.372850028	0.017651429	NA	up_regulated
stress	under	CvsDi	TRINITY_DN679434_c0_g1	15.00819299	7.094155958	3.330068571	2.130333297	0.033144106	NA	up_regulated
stress	under	CvsDi	TRINITY_DN38375_c0_g1	15.40841147	7.13211971	3.326359564	2.144121696	0.032023137	NA	up_regulated
stress	under	CvsDi	TRINITY_DN80787_c1_g1	16.60906691	7.240361288	3.316496407	2.183135574	0.029025827	0.198280917	up_regulated
stress	under	CvsDi	TRINITY_DN12968_c0_g1	17.43214023	7.310183626	2.968828202	2.462312781	0.013804422	0.146121128	up_regulated
stress	under	CvsDi	TRINITY_DN12694_c0_g1	18.06264975	7.361433291	2.901188877	2.537385052	0.011168404	0.132698631	up_regulated
stress	under	CvsDi	TRINITY_DN19428_c0_g1	18.47795913	7.394221042	2.881166025	2.566398804	0.01027606	0.132419799	up_regulated
stress	under	CvsDi	TRINITY_DN257495_c0_g4	19.09668614	7.441411827	3.048516519	2.440994425	0.014646881	0.148452241	up_regulated
stress	under	CvsDi	TRINITY_DN1794_c11_g1	19.21048703	7.450263985	3.300160604	2.257545883	0.023973986	0.180165443	up_regulated
stress	under	CvsDi	TRINITY_DN66082_c0_g1	20.21103323	7.523506329	3.295258421	2.283130901	0.022422659	0.173152754	up_regulated
stress	under	CvsDi	TRINITY_DN113017_c0_g1	20.62634261	7.552893903	2.987136726	2.52847278	0.011455997	0.132698631	up_regulated
stress	under	CvsDi	TRINITY_DN1083884_c0_g1	20.81136095	7.565730836	3.292607212	2.297793314	0.021573556	0.170659102	up_regulated
stress	under	CvsDi	TRINITY_DN55030_c0_g1	21.41168867	7.606754553	3.290149863	2.311978138	0.020778888	0.167089737	up_regulated
stress	under	CvsDi	TRINITY_DN14448_c0_g1	21.81190715	7.633469615	3.28861091	2.321183571	0.020276938	0.164412176	up_regulated
stress	under	CvsDi	TRINITY_DN28119_c0_g1	22.01139727	7.646368072	3.290248037	2.323948829	0.020128237	0.164412176	up_regulated
stress	under	CvsDi	TRINITY_DN12365_c0_g1	35.01911698	8.31644732	3.263269183	2.54850178	0.010818673	0.132698631	up_regulated
stress	under	CvsDr	TRINITY_DN341950_c0_g1	100.6415073	4.724952314	2.298128514	2.056000039	0.039782512	0.214012494	up_regulated
stress	under	CvsDr	TRINITY_DN1682_c0_g1	72.51484865	5.228085426	2.362737699	2.212723582	0.026916709	0.163947225	up_regulated
stress	under	CvsDr	TRINITY_DN803_c12_g1	15.63211307	5.624717843	2.857958278	1.968089557	0.049057734	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1810_c1_g3	33.65752034	5.668039772	2.777033379	2.041041283	0.041246724	0.220281416	up_regulated
stress	under	CvsDr	TRINITY_DN8655_c1_g1	7.662617341	6.054054809	3.175387011	1.90655652	0.05657804	NA	up_regulated
stress	under	CvsDr	TRINITY_DN35414_c0_g2	8.362318742	6.180805815	3.163399361	1.953849359	0.050719035	NA	up_regulated
stress	under	CvsDr	TRINITY_DN71644_c0_g1	8.505259875	6.201894196	3.154607682	1.965979551	0.049300973	NA	up_regulated
stress	under	CvsDr	TRINITY_DN85121_c0_g1	8.588460291	6.215093382	3.174037913	1.95810307	0.05021792	NA	up_regulated
stress	under	CvsDr	TRINITY_DN56911_c0_g1	8.713260915	6.234906451	3.2645263	1.909896223	0.056146576	NA	up_regulated
stress	under	CvsDr	TRINITY_DN13992_c0_g1	8.842289255	6.262921891	3.313628778	1.890049342	0.058751361	NA	up_regulated
stress	under	CvsDr	TRINITY_DN26127_c0_g1	9.210280465	6.320609857	3.157196247	2.001969267	0.045288037	NA	up_regulated
stress	under	CvsDr	TRINITY_DN51446_c0_g1	9.697753111	6.390005179	3.17902349	2.010052835	0.044425597	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1026_c6_g1	56.40095213	6.422851622	3.182085892	2.018440683	0.043545386	0.22760957	up_regulated
stress	under	CvsDr	TRINITY_DN31659_c0_g1	10.0998424	6.453586383	3.142123861	2.053893057	0.039986043	NA	up_regulated
stress	under	CvsDr	TRINITY_DN77467_c0_g2	10.41982274	6.499305644	3.234987791	2.009066514	0.04453008	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1177410_c0_g1	10.60436408	6.522325455	3.094971076	2.10739464	0.035083386	NA	up_regulated
stress	under	CvsDr	TRINITY_DN77002_c0_g1	12.30560671	6.738501797	3.565739039	1.889791071	0.058785908	NA	up_regulated
stress	under	CvsDr	TRINITY_DN58403_c0_g1	13.13229168	6.828125665	3.587890414	1.903103183	0.057027083	NA	up_regulated
stress	under	CvsDr	TRINITY_DN109376_c0_g1	14.34824462	6.956038248	3.577775649	1.94423545	0.051867065	NA	up_regulated
stress	under	CvsDr	TRINITY_DN64184_c0_g1	14.38125122	6.963085369	3.547138599	1.963014744	0.049644461	NA	up_regulated
stress	under	CvsDr	TRINITY_DN760592_c0_g1	14.5914352	6.980314549	3.57600017	1.951989434	0.050939459	NA	up_regulated
stress	under	CvsDr	TRINITY_DN5724_c1_g1	16.07185548	7.123243467	3.076789696	2.315154486	0.020604479	NA	up_regulated
stress	under	CvsDr	TRINITY_DN16478_c0_g3	16.13691538	7.129296694	3.11203274	2.290881006	0.021970297	NA	up_regulated
stress	under	CvsDr	TRINITY_DN143055_c2_g1	16.16037508	7.131161989	3.535801484	2.01684456	0.043711737	NA	up_regulated
stress	under	CvsDr	TRINITY_DN19332_c0_g1	16.4345275	7.153976306	3.018661444	2.369916746	0.017792091	NA	up_regulated
stress	under	CvsDr	TRINITY_DN8736_c0_g1	46.31810569	7.168674358	2.950144779	2.429939849	0.015101329	0.11593416	up_regulated
stress	under	CvsDr	TRINITY_DN2815_c3_g4	16.64675626	7.174255664	3.190480746	2.248644086	0.024535149	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1632280_c0_g1	17.02334107	7.202949874	3.561698352	2.022335741	0.043141679	NA	up_regulated
stress	under	CvsDr	TRINITY_DN3612_c2_g1	17.99610342	7.283200096	3.557359767	2.047361125	0.04062264	NA	up_regulated
stress	under	CvsDr	TRINITY_DN3423_c0_g1	19.51703243	7.40345415	3.235540035	2.288166448	0.02212783	NA	up_regulated
stress	under	CvsDr	TRINITY_DN109422_c1_g1	20.45992442	7.471161684	3.518644235	2.123306929	0.03372814	NA	up_regulated
stress	under	CvsDr	TRINITY_DN3382_c7_g1	22.3873086	7.60092864	3.513866882	2.163123674	0.030531669	NA	up_regulated
stress	under	CvsDr	TRINITY_DN39465_c0_g1	23.34629633	7.659036658	3.542012791	2.162340203	0.030591963	NA	up_regulated
stress	under	CvsDr	TRINITY_DN2386_c7_g1	23.58948691	7.673998699	3.541554602	2.166844666	0.0302467	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1648240_c0_g1	25.04863044	7.760650377	3.539107399	2.192827033	0.028319843	0.166973797	up_regulated
stress	under	CvsDr	TRINITY_DN57265_c0_g1	25.20425472	7.771779164	3.508868517	2.214896091	0.026767197	0.163947225	up_regulated
stress	under	CvsDr	TRINITY_DN14644_c1_g1	27.57751134	7.901366963	2.994292997	2.638808884	0.008319787	0.095807546	up_regulated
stress	under	CvsDr	TRINITY_DN9947_c0_g1	29.35554373	7.991589667	3.504357895	2.280471888	0.022579716	0.149921179	up_regulated
stress	under	CvsDr	TRINITY_DN124934_c0_g1	32.32075017	8.130330122	3.502495945	2.321296084	0.020270869	0.141046167	up_regulated
stress	under	CvsDr	TRINITY_DN5756_c0_g3	33.31711038	8.172443418	3.531771757	2.31397836	0.020668909	0.141046167	up_regulated
stress	under	CvsDr	TRINITY_DN1065_c0_g1	98.3837409	8.303461093	2.996349817	2.77119215	0.005585146	0.076888617	up_regulated
stress	under	CvsDr	TRINITY_DN29226_c0_g1	38.79305722	8.393233828	2.95000999	2.845154374	0.00443899	0.072830271	up_regulated
stress	under	CvsDr	TRINITY_DN17149_c0_g3	57.52500488	8.961669993	3.503547949	2.557884214	0.010531115	0.099505535	up_regulated
stress	under	CvsDr	TRINITY_DN3929_c0_g1	62.98636197	9.091596242	3.534112955	2.572525654	0.010095947	0.099501893	up_regulated
stress	under	CvsDr	TRINITY_DN187065_c0_g1	64.19671937	9.11992723	3.505561164	2.601559865	0.009280087	0.099501893	up_regulated
stress	under	CvsDr	TRINITY_DN58887_c0_g1	64.49324001	9.12657348	3.505655548	2.603385688	0.009230804	0.099501893	up_regulated
stress	under	CvsDr	TRINITY_DN4640_c0_g1	101.7065808	22.51893133	3.51819125	6.400712676	1.55E-10	8.34E-09	up_regulated
stress	under	CvsDxD	TRINITY_DN1290_c0_g1	1157.864747	3.958230881	2.088955467	1.894837369	0.058113951	0.237142412	up_regulated
stress	under	CvsDxD	TRINITY_DN3496_c0_g1	159.2718194	4.109686877	2.085310688	1.970779175	0.04874914	0.21511975	up_regulated
stress	under	CvsDxD	TRINITY_DN30848_c0_g1	63.14519751	4.187689488	2.197192412	1.905927521	0.05665961	0.234081154	up_regulated
stress	under	CvsDxD	TRINITY_DN341950_c0_g1	63.65104914	4.235659148	2.217667597	1.909961238	0.056138204	0.234081154	up_regulated
stress	under	CvsDxD	TRINITY_DN2815_c0_g1	130.3639674	4.260243378	2.168875959	1.964263268	0.049499569	0.21716863	up_regulated
stress	under	CvsDxD	TRINITY_DN1266006_c0_g1	31.94109133	4.495101808	2.342686188	1.918781026	0.055012052	0.233201153	up_regulated
stress	under	CvsDxD	TRINITY_DN47749_c0_g1	20.56059303	4.545909386	2.36701329	1.920525501	0.054791555	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN63119_c0_g1	27.7808005	4.598998895	2.363970273	1.945455468	0.051720184	0.224317828	up_regulated
stress	under	CvsDxD	TRINITY_DN26857_c0_g1	68.27124546	4.615536722	2.316230217	1.992693424	0.046295028	0.205484948	up_regulated
stress	under	CvsDxD	TRINITY_DN19689_c0_g1	141.1623797	4.993578258	2.364827361	2.111603722	0.034720453	0.16574103	up_regulated
stress	under	CvsDxD	TRINITY_DN3439_c3_g1	142.7776884	4.994371753	2.259740383	2.210152897	0.027094553	0.138018561	up_regulated
stress	under	CvsDxD	TRINITY_DN147385_c0_g1	44.57842779	5.036909543	2.615036796	1.926133334	0.054087727	0.231935509	up_regulated
stress	under	CvsDxD	TRINITY_DN786_c0_g2	84.03390768	5.12329754	2.254338747	2.272638727	0.02304796	0.125935441	up_regulated
stress	under	CvsDxD	TRINITY_DN29820_c0_g1	19.53134305	5.180772569	2.631838026	1.968499777	0.049010562	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1682_c0_g1	67.88344926	5.2334347	2.38348609	2.195705996	0.028112994	0.139462498	up_regulated
stress	under	CvsDxD	TRINITY_DN3673_c0_g1	144.0183431	5.260305897	2.535555151	2.07461703	0.038022042	0.174901395	up_regulated
stress	under	CvsDxD	TRINITY_DN1022055_c0_g1	23.47411397	5.455223808	2.716385802	2.00826547	0.044615089	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN4585_c0_g2	46.3909854	5.46077337	2.597086667	2.1026535	0.03549607	0.16630566	up_regulated
stress	under	CvsDxD	TRINITY_DN5756_c0_g5	31.96400004	5.593977912	2.660082574	2.102933934	0.035471545	0.16630566	up_regulated
stress	under	CvsDxD	TRINITY_DN44930_c0_g1	15.32621764	5.599394049	2.855696912	1.96078023	0.049904665	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN296357_c0_g1	15.05317007	5.71689114	2.951418302	1.936997929	0.052745596	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN48680_c0_g1	16.88933883	5.736238601	2.983380146	1.922731372	0.054513795	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1810_c1_g3	35.83924184	5.762529549	2.654943854	2.170490174	0.029969732	0.145814274	up_regulated
stress	under	CvsDxD	TRINITY_DN5467_c0_g1	80.04596188	5.782931635	2.241101321	2.580397227	0.009868673	0.086095661	up_regulated
stress	under	CvsDxD	TRINITY_DN1795_c2_g4	141.342341	5.92404902	2.664024452	2.223721714	0.026167173	0.136033456	up_regulated
stress	under	CvsDxD	TRINITY_DN63156_c0_g2	6.684805398	5.965544185	3.141272028	1.899085508	0.057553234	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN13166_c0_g1	34.02934762	6.003531938	2.769864616	2.167445984	0.030200864	0.146002904	up_regulated
stress	under	CvsDxD	TRINITY_DN61288_c1_g2	6.922615325	6.018001916	3.147222881	1.912162609	0.05585534	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN38375_c2_g1	35.39263529	6.054946437	2.612819985	2.317399006	0.020482006	0.120021733	up_regulated
stress	under	CvsDxD	TRINITY_DN135256_c0_g1	7.174171753	6.069839077	3.216304635	1.887209007	0.059132219	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN114897_c0_g1	57.08902418	6.178920363	2.623151861	2.355532844	0.018496162	0.114134852	up_regulated
stress	under	CvsDxD	TRINITY_DN4043_c3_g2	7.823682576	6.194140454	3.089887571	2.004649138	0.04500057	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN28942_c0_g1	254.4761923	6.197505248	2.349133693	2.638208829	0.008334524	0.076215707	up_regulated
stress	under	CvsDxD	TRINITY_DN1026_c0_g1	40.15038429	6.243070983	2.698202601	2.313788809	0.02067931	0.120021733	up_regulated
stress	under	CvsDxD	TRINITY_DN358003_c0_g1	8.134691486	6.250992177	3.189809993	1.959675401	0.050033742	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN527436_c0_g1	8.485736512	6.311920802	3.237002247	1.949927841	0.05118472	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN186521_c0_g1	8.685917096	6.342996619	3.159328497	2.00770405	0.04467475	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN54996_c0_g1	9.095211218	6.411904776	3.17097348	2.022061936	0.043169954	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN77693_c0_g1	9.428043267	6.461316624	3.230083935	2.000355642	0.045461875	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN36834_c0_g1	9.513785364	6.47485108	3.04978414	2.123052249	0.033749474	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN11818_c3_g1	25.56510451	6.489434035	2.715475662	2.389796427	0.016857714	0.105743842	up_regulated
stress	under	CvsDxD	TRINITY_DN32851_c0_g1	9.745925428	6.509289875	3.103742343	2.097239125	0.035972413	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN11579_c0_g1	9.751595291	6.511588147	3.024140142	2.153203172	0.031302714	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN40576_c0_g1	10.2890744	6.58773288	3.055054962	2.156338581	0.031057237	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN133849_c0_g1	10.5875402	6.628924037	3.500436684	1.89374202	0.058259261	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1340517_c0_g1	10.5875402	6.628924037	3.500436684	1.89374202	0.058259261	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN31154_c0_g1	10.64699268	6.636923752	3.090576276	2.147471267	0.031755782	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN133093_c0_g1	10.71898827	6.647772926	2.998203236	2.217252268	0.026605858	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN77467_c0_g2	10.80593376	6.658382245	3.065653473	2.171929183	0.029861005	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN8736_c0_g1	31.6962926	6.671076664	2.709236501	2.462345632	0.013803158	0.095241788	up_regulated
stress	under	CvsDxD	TRINITY_DN1823386_c0_g1	11.00491096	6.684581272	3.208625907	2.08331587	0.037222441	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN180460_c0_g1	11.01625068	6.688186774	3.143749967	2.12745506	0.033382292	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN63413_c0_g1	11.08257642	6.696792119	3.091252523	2.166368509	0.030283038	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1399958_c0_g1	11.1420289	6.70437581	3.48231651	1.925263195	0.05419644	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN283420_c0_g1	11.14890215	6.705323561	3.061763886	2.190019809	0.0285228	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN192722_c0_g1	11.20835463	6.71311317	3.141643153	2.136815941	0.032612965	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN38474_c1_g1	11.34100609	6.729961431	3.059620635	2.199606498	0.027834826	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN159957_c0_g1	11.36729571	6.733412236	3.194165041	2.108035167	0.035027948	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN13616_c1_g2	11.51489706	6.750061568	3.068363702	2.199889656	0.027814725	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN22214_c0_g1	11.52623679	6.753258597	3.477260506	1.942120409	0.052122525	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN39369_c0_g1	11.52623679	6.753258597	3.477260506	1.942120409	0.052122525	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN31659_c0_g1	11.92539457	6.800883435	3.007364202	2.261409985	0.023733881	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1878223_c0_g1	12.16440789	6.829329015	3.479748963	1.962592442	0.04969355	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN55719_c0_g1	12.32334897	6.847920173	3.14186391	2.179572499	0.029289165	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN19850_c6_g1	12.61494151	6.881822449	3.474944512	1.980412184	0.047657232	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN72369_c1_g1	12.67886047	6.890690688	3.464236323	1.989093712	0.04669086	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN186474_c0_g1	12.77388259	6.899751823	3.138245469	2.1986017	0.027906256	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN16143_c1_g2	12.96478315	6.922210933	2.962570643	2.336555569	0.019462305	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN4043_c1_g1	13.10310448	6.938214292	3.032805253	2.287721668	0.022153735	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN6998_c0_g1	126.7064261	6.938844164	3.08007835	2.252814174	0.024270868	0.129729499	up_regulated
stress	under	CvsDxD	TRINITY_DN35475_c0_g1	13.25517231	6.954789296	3.45873192	2.01079166	0.044347467	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN56764_c0_g3	13.51600876	6.981405473	3.466481363	2.013974616	0.044012199	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1050344_c0_g1	13.74127557	7.00526319	3.46457583	2.021968499	0.043179606	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN31568_c0_g1	134.063584	7.012718635	2.611953851	2.684855489	0.007256117	0.068954502	up_regulated
stress	under	CvsDxD	TRINITY_DN1063_c1_g2	36.80541862	7.020390484	2.763557279	2.540345568	0.011074299	0.088322486	up_regulated
stress	under	CvsDxD	TRINITY_DN291754_c0_g1	13.96654239	7.028732839	3.462745912	2.029814782	0.04237537	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN706070_c0_g1	14.21569204	7.055670265	3.450758084	2.044672531	0.040887154	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN7640_c3_g1	14.60677319	7.094911504	3.033500447	2.338852961	0.019343045	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN168873_c0_g1	15.9939437	7.224366797	3.449128336	2.094548562	0.036211142	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN16580_c0_g1	16.44327394	7.264808107	2.928668478	2.480583979	0.013116736	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1611870_c0_g1	44.40418145	7.296814046	3.21798671	2.267509068	0.023359146	0.126639939	up_regulated
stress	under	CvsDxD	TRINITY_DN16478_c0_g1	17.00463589	7.314149773	3.07469537	2.378820954	0.01736811	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN49415_c6_g1	18.05777097	7.400660991	3.429212014	2.158122904	0.030918277	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN24033_c0_g1	18.11275698	7.404341514	2.915494539	2.539651993	0.011096282	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN55762_c1_g1	18.38493316	7.425595331	2.937683605	2.527704249	0.011481102	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1803132_c0_g1	19.04131723	7.476023318	2.991115692	2.499409614	0.012440043	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN105850_c1_g1	19.72399092	7.526802163	3.078502315	2.444955824	0.014486989	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1842629_c0_g1	20.04513617	7.551371662	3.097378287	2.437988183	0.014769257	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN6578_c0_g2	20.15837127	7.559341579	2.941554077	2.569846204	0.010174367	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN280_c14_g1	20.68777374	7.596880273	3.044170914	2.495549852	0.012576208	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN4778_c2_g1	23.05247358	7.752841165	3.414915732	2.270287694	0.023190133	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN28119_c0_g1	23.24457753	7.764810149	3.414546388	2.27403856	0.022963668	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN2386_c0_g2	26.03146167	7.927247751	3.07101508	2.581311894	0.009842561	0.086095661	up_regulated
stress	under	CvsDxD	TRINITY_DN9743_c0_g1	26.5814839	7.957499755	3.41940223	2.327161071	0.019956694	0.11833696	up_regulated
stress	under	CvsDxD	TRINITY_DN1044_c14_g1	27.25247079	7.994351372	3.02153005	2.645795752	0.008149901	0.075436278	up_regulated
stress	under	CvsDxD	TRINITY_DN280_c0_g2	276.0125942	8.058061996	2.524355256	3.192126773	0.001412293	0.024928619	up_regulated
stress	under	CvsDxD	TRINITY_DN3443_c9_g1	29.58194806	8.111858587	2.916142794	2.78170829	0.005407362	0.063141349	up_regulated
stress	under	CvsDxD	TRINITY_DN39465_c0_g1	30.4587854	8.154766209	2.977441508	2.73885018	0.006165446	0.064459963	up_regulated
stress	under	CvsDxD	TRINITY_DN16775_c0_g1	32.1731181	8.232940179	2.997583865	2.746525385	0.006023022	0.064459963	up_regulated
stress	under	CvsDxD	TRINITY_DN39544_c0_g1	34.1742298	8.32001344	3.099496055	2.684311672	0.007267932	0.068954502	up_regulated
stress	under	CvsDxD	TRINITY_DN101231_c1_g2	34.91635597	8.351068314	3.413821044	2.446252515	0.014434986	0.097822808	up_regulated
stress	under	CvsDxD	TRINITY_DN1951_c0_g3	220.5733101	8.556735617	2.695390163	3.174581452	0.001500528	0.025155425	up_regulated
stress	under	CvsDxD	TRINITY_DN4123_c1_g1	40.28237628	8.557962075	3.039308036	2.815760026	0.0048662	0.059571706	up_regulated
stress	under	CvsDxD	TRINITY_DN47749_c0_g2	41.22382672	8.590686381	3.412996986	2.517050679	0.011834181	0.091654521	up_regulated
stress	under	CvsDxD	TRINITY_DN280_c3_g1	41.87866034	8.613925834	3.403294591	2.531055012	0.011372001	0.088982978	up_regulated
stress	under	CvsDxD	TRINITY_DN1251980_c0_g1	47.78938009	8.80437653	2.868449666	3.069385053	0.002144999	0.031922637	up_regulated
stress	under	CvsDxD	TRINITY_DN1762_c0_g2	134.786508	8.899764469	2.741797615	3.245959665	0.001170554	0.021669528	up_regulated
stress	under	CvsDxD	TRINITY_DN803_c12_g1	143.2722445	8.98802927	2.744010609	3.275508207	0.00105472	0.020054689	up_regulated
stress	under	CvsDxD	TRINITY_DN763605_c0_g1	66.49545849	9.280934382	2.921540235	3.176726533	0.001489474	0.025155425	up_regulated
stress	under	CvsDxD	TRINITY_DN523_c2_g1	101.2868926	9.887982608	2.899444917	3.410301934	0.00064891	0.013681185	up_regulated
stress	under	CvsDxD	TRINITY_DN51446_c0_g1	137.0307697	10.32400173	2.953871875	3.495074319	0.00047393	0.011990423	up_regulated
stress	under	CvsDxD	TRINITY_DN280_c8_g1	155.3263506	10.50478461	3.033762752	3.462625615	0.000534932	0.012303439	up_regulated
stress	under	CvsDxD	TRINITY_DN3612_c2_g1	473.5233834	12.11278501	3.055262403	3.964564548	7.35E-05	0.002325388	up_regulated
stress	under	CvsDxD	TRINITY_DN3929_c0_g1	2083.827642	14.25051563	3.053407072	4.667086731	3.06E-06	0.000122039	up_regulated
   ")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


HSP70_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

HSP70_reg

HSP70_reg_final <- HSP70_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

HSP70_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

HSP70_reg_final_plot <- HSP70_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                               panel.background = element_rect(fill = "white"), 
                                                                                               panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                               axis.title.x = element_text(color="black", vjust=1),
                                                                                               axis.title.y = element_text(color="black" , vjust=1)) 






HSP70_reg_final_plot


HSP70_reg_final_plot_facet <- HSP70_reg_final_plot + facet_grid( ~ location, scales='free_y') #scales='fixed' makes that all y-axes are the same

HSP70_reg_final_plot_facet

jpeg("HSP70_regulation_FACET.jpeg",height=6,width=10,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(HSP70_reg_final_plot_facet)
dev.off()






################ heat shock protein 90 


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	inter	CvsDr	TRINITY_DN1955_c0_g1	149.8671424	-10.54637492	3.327616103	-3.169348444	0.001527811	0.025433562	down_regulated
stress	inter	CvsDr	TRINITY_DN1955_c0_g2	55.48093547	-9.112762281	3.575371298	-2.548759701	0.010810676	0.09027918	down_regulated
stress	inter	CvsDr	TRINITY_DN879523_c0_g1	53.93365839	-9.071957138	3.575123964	-2.537522399	0.011164022	0.09027918	down_regulated
stress	inter	CvsDr	TRINITY_DN3020_c0_g1	43.32375838	-8.755938353	3.574594257	-2.449491529	0.014305807	0.101213588	down_regulated
stress	inter	CvsDr	TRINITY_DN4458_c12_g1	31.16658128	-8.280793817	3.579450063	-2.313426273	0.020699214	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN28253_c0_g1	24.97747294	-7.96143156	3.587651194	-2.219120848	0.026478504	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN9850_c0_g1	19.23044377	-7.584209055	3.603999633	-2.104386744	0.035344725	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN898327_c0_g1	17.4621271	-7.445051409	3.612219878	-2.061073706	0.039296009	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN47704_c1_g4	17.02004793	-7.408058667	3.614628686	-2.04946602	0.040416567	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN9329_c0_g2	15.85283564	-7.305504955	3.268971943	-2.234801975	0.025430352	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN15767_c0_g1	15.49581798	-7.272738438	3.15609776	-2.304345109	0.021203279	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN13085_c1_g1	15.18741217	-7.243623758	3.627183233	-1.997038278	0.045821025	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN81907_c0_g1	13.26237501	-7.048176871	3.643569373	-1.934415445	0.053062069	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN13209_c0_g1	13.04133543	-7.023930336	3.645911868	-1.92652225	0.054039196	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN76727_c0_g1	9.585367051	-6.579676297	3.235540287	-2.033563397	0.041995635	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN61989_c0_g1	9.15481145	-6.5133427	3.392347833	-1.920010276	0.054856601	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN1322_c0_g2	9.131764319	-6.509749283	3.227076345	-2.017228162	0.043671708	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN614254_c0_g1	8.429465445	-6.394370445	3.236680052	-1.975595469	0.048200612	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1955_c0_g2	65.29924645	-9.638795209	3.489582231	-2.762163082	0.005741979	0.084430584	down_regulated
stress	inter	CvsDi	TRINITY_DN879523_c0_g1	63.47815193	-9.597985974	3.489393542	-2.750617223	0.00594831	0.084430584	down_regulated
stress	inter	CvsDi	TRINITY_DN4458_c12_g1	36.68204681	-8.806716945	3.495313988	-2.519578206	0.011749554	0.10636438	down_regulated
stress	inter	CvsDi	TRINITY_DN28253_c0_g1	29.39766872	-8.487294423	3.504477504	-2.421843031	0.015442019	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1955_c0_g1	176.9950679	-8.101324038	2.921891592	-2.772629916	0.005560532	0.084430584	down_regulated
stress	inter	CvsDi	TRINITY_DN898327_c0_g1	20.55235247	-7.970788403	3.531142237	-2.2572833	0.023990378	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN15767_c0_g1	17.92675209	-7.773559797	3.087778117	-2.517525386	0.011818245	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN946_c6_g1	17.6097571	-7.747846373	3.076906696	-2.518063477	0.011800205	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN124400_c0_g1	16.24563006	-7.632465954	3.09743121	-2.464127671	0.013734718	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN13209_c0_g1	15.34922526	-7.549533487	3.567169017	-2.116393546	0.03431135	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN31404_c0_g1	14.02440669	-7.419328933	3.117046498	-2.380243265	0.017301212	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN16689_c0_g2	13.90422282	-7.407984622	3.575862509	-2.071663719	0.038296813	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1211326_c0_g1	11.44687986	-7.126173368	3.620560567	-1.96825139	0.04903912	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN8502_c0_g2	11.2523272	-7.101814216	3.130089072	-2.268885662	0.02327528	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN6617_c0_g1	10.92656714	-7.059032442	3.630926191	-1.944140991	0.051878451	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN13580_c0_g1	10.40625441	-6.988614206	3.642420386	-1.918673153	0.055025711	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN69849_c0_g1	10.14609805	-6.952072957	3.648644748	-1.905384995	0.056730044	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN23380_c2_g1	10.11767874	-6.947909601	3.340457272	-2.079927697	0.037532165	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN1138519_c0_g1	9.885941694	-6.914582212	3.655220556	-1.891700407	0.058530909	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN47579_c2_g1	9.885941694	-6.914582212	3.655220556	-1.891700407	0.058530909	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN3020_c0_g1	51.41562986	-6.893425333	3.17252367	-2.172852294	0.029791436	0.22278813	down_regulated
stress	inter	CvsDi	TRINITY_DN61989_c0_g1	9.529638238	-6.863330386	3.345935945	-2.051243807	0.04024321	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN38927_c0_g1	8.399372317	-6.681257957	3.323052598	-2.010578455	0.044370002	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN47704_c1_g2	7.345599257	-6.48613671	3.284560552	-1.974735009	0.048298228	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN1955_c0_g1	149.7905294	-10.32370856	3.61426089	-2.85638167	0.004284997	0.037902473	down_regulated
stress	inter	CvsDxD	TRINITY_DN1955_c0_g2	55.46536187	-8.890347155	3.900673907	-2.279182358	0.022656227	0.103211702	down_regulated
stress	inter	CvsDxD	TRINITY_DN879523_c0_g1	53.91851911	-8.849537997	3.900197546	-2.268997376	0.023268485	0.104344614	down_regulated
stress	inter	CvsDxD	TRINITY_DN3020_c0_g1	43.31159732	-8.5334839	3.897566922	-2.189438712	0.028564969	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN4458_c12_g1	31.15783277	-8.058269336	3.897939159	-2.067315319	0.038704452	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN28253_c0_g1	24.97046172	-7.738845652	3.901983107	-1.983310906	0.047332712	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN9850_c0_g1	19.22504575	-7.361531934	3.9119032	-1.881828756	0.05985927	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN139_c0_g1	81.41918423	4.759687823	2.342190225	2.032152543	0.042138216	0.25924163	up_regulated
stress	inter	CvsDr	TRINITY_DN17920_c1_g2	7.897712618	6.594140576	3.249807615	2.029086444	0.042449486	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN4247_c0_g2	8.961563462	6.775337604	3.388656508	1.999417052	0.045563248	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN61266_c1_g1	10.15688274	6.956220375	3.680658491	1.889939094	0.058766106	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN40315_c2_g1	12.18825929	7.219385174	3.64874333	1.978594963	0.047861627	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN468_c4_g5	13.59804757	7.37775845	3.131406972	2.356052253	0.018470319	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN2932_c2_g1	44.22986648	9.079871874	3.55925592	2.551059008	0.010739614	0.09027918	up_regulated
stress	inter	CvsDr	TRINITY_DN8406_c0_g1	129.1447449	9.182359695	3.097943241	2.964018053	0.003036505	0.042966542	up_regulated
stress	inter	CvsDr	TRINITY_DN1093_c1_g1	51.13279362	9.289067466	3.559534301	2.609629991	0.00906402	0.09027918	up_regulated
stress	inter	CvsDr	TRINITY_DN689_c0_g2	60.8480244	9.539994978	3.561271368	2.678817195	0.007388272	0.087120038	up_regulated
stress	inter	CvsDi	TRINITY_DN11762_c1_g1	30.42663405	4.979442176	2.589115695	1.923221193	0.054452277	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN162830_c0_g2	11.19442691	6.952788316	3.630222792	1.915251133	0.055460484	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN1204136_c0_g1	12.11202189	7.067414544	3.607811021	1.958920382	0.050122112	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN775023_c0_g1	16.57434575	7.519810572	3.555439895	2.115015524	0.034428624	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN13045_c0_g4	17.95772651	7.634779241	3.550924418	2.150082159	0.031548717	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN35315_c0_g1	18.27427864	7.660641938	3.543123093	2.162115664	0.030609262	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN52602_c0_g1	18.27427864	7.660641938	3.543123093	2.162115664	0.030609262	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN6873_c0_g1	22.40979566	7.95459535	2.989405405	2.660928938	0.00779254	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN3481_c1_g1	23.32172273	8.011917031	3.524829823	2.272993998	0.023026542	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN35945_c0_g1	20.313444	6.650141902	3.196330254	2.080555316	0.037474627	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN263_c11_g1	41.12942637	6.731015571	3.119108566	2.157993359	0.030928348	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN57559_c0_g1	8.156722732	6.762652727	3.493600868	1.935725626	0.052901314	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN58548_c0_g2	8.412917559	6.807405398	3.518976329	1.934484566	0.053053578	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN954144_c0_g1	12.97727463	7.432571066	3.911441946	1.900212548	0.057405232	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN36036_c0_g1	13.06314786	7.441474706	3.913991461	1.901249602	0.057269327	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN373713_c0_g1	13.59524009	7.499671432	3.906875759	1.919608376	0.054907385	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN29898_c1_g1	13.90422282	7.532086399	3.904784954	1.928937569	0.053738613	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1407801_c0_g1	14.21320555	7.56378903	3.902810542	1.938036435	0.052618778	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN8072_c0_g1	14.21320555	7.56378903	3.902810542	1.938036435	0.052618778	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN36178_c0_g1	15.02262004	7.643143963	3.901346816	1.959103952	0.050100614	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN764466_c0_g1	15.14015373	7.654919571	3.897510035	1.964053845	0.049523848	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN547514_c0_g1	15.52726501	7.691139856	3.366891659	2.284344326	0.022351303	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN175631_c0_g1	16.65551352	7.792030667	3.893710718	2.001183763	0.04537259	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN11463_c0_g1	17.51207784	7.864417894	3.415204808	2.302766111	0.021292006	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1566_c3_g1	18.64243296	7.95476264	3.361908234	2.366145084	0.017974399	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN10292_c0_g1	19.2681431	8.002277419	3.885099705	2.059735406	0.039423843	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN47547_c0_g1	260.2436171	8.626564089	3.209825929	2.687548883	0.007197857	0.055832023	up_regulated
stress	inter	CvsDxD	TRINITY_DN468_c0_g1	883.2955469	8.96595536	3.601397343	2.489576824	0.012789527	0.07979553	up_regulated
stress	inter	CvsDxD	TRINITY_DN908043_c0_g1	44.41470273	9.207198264	3.870895709	2.378570479	0.017379914	0.082986683	up_regulated
stress	inter	CvsDxD	TRINITY_DN47547_c1_g1	77.36395915	10.00786736	3.510294175	2.851005317	0.004358124	0.037902473	up_regulated
stress	inter	CvsDxD	TRINITY_DN1098572_c0_g1	77.07257239	22.7120071	3.881490938	5.851361619	4.88E-09	5.64E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN9021_c1_g1	88.82940546	22.90936071	3.885685563	5.895834941	3.73E-09	5.35E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN946_c2_g1	233.1771893	23.34064257	3.923043209	5.949626687	2.69E-09	5.20E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN18642_c0_g1	146.3072561	23.60130982	3.903511174	6.046174525	1.48E-09	3.87E-08	up_regulated
stress	inter	CvsDxD	TRINITY_DN235_c0_g2	152.83883	23.6659421	3.905247666	6.060036167	1.36E-09	3.87E-08	up_regulated
stress	under	CvsDi	TRINITY_DN6515_c1_g1	384.0485675	-25.60372382	3.294217075	-7.772324422	7.71E-15	6.47E-12	down_regulated
stress	under	CvsDi	TRINITY_DN85_c0_g4	305.6551486	-25.29021077	3.283198331	-7.70291899	1.33E-14	6.47E-12	down_regulated
stress	under	CvsDi	TRINITY_DN23199_c0_g1	189.5167501	-24.63983092	3.261898892	-7.553830373	4.23E-14	8.52E-12	down_regulated
stress	under	CvsDi	TRINITY_DN1838_c0_g1	211.6224242	-10.02371615	2.537918298	-3.949581892	7.83E-05	0.004231892	down_regulated
stress	under	CvsDi	TRINITY_DN1347375_c0_g1	58.36698557	-9.603637622	2.745218654	-3.498314282	0.000468209	0.02277837	down_regulated
stress	under	CvsDi	TRINITY_DN184957_c1_g1	16.22535974	-7.75568412	2.878576862	-2.694277239	0.007054146	NA	down_regulated
stress	under	CvsDi	TRINITY_DN9641_c0_g1	16.05860173	-7.744145139	2.943690673	-2.630760497	0.008519405	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1435494_c0_g1	8.561253607	-6.832014461	3.065864849	-2.228413449	0.025852957	NA	down_regulated
stress	under	CvsDi	TRINITY_DN97047_c0_g1	14.5324743	-5.112433527	2.424185637	-2.10892823	0.034950777	NA	down_regulated
stress	under	CvsDi	TRINITY_DN63454_c0_g1	26.24501865	-4.987555945	2.51853936	-1.980336708	0.047665707	0.28279715	down_regulated
stress	under	CvsDr	TRINITY_DN1347375_c0_g1	58.0631615	-9.698819018	2.967463673	-3.268386773	0.001081624	0.030659893	down_regulated
stress	under	CvsDr	TRINITY_DN27084_c0_g1	89.82854523	-8.725935652	2.790234166	-3.127313026	0.00176412	0.037147325	down_regulated
stress	under	CvsDr	TRINITY_DN63454_c0_g1	28.67977363	-8.681706568	3.086062688	-2.813198384	0.004905138	0.076607612	down_regulated
stress	under	CvsDr	TRINITY_DN73884_c0_g1	24.81422029	-8.472215915	3.017472552	-2.807719298	0.00498937	0.076607612	down_regulated
stress	under	CvsDr	TRINITY_DN7611_c0_g1	23.99674225	-8.423953175	2.998540163	-2.809351457	0.004964142	NA	down_regulated
stress	under	CvsDr	TRINITY_DN184957_c1_g1	16.13316728	-7.850925466	3.072474369	-2.555245227	0.010611301	NA	down_regulated
stress	under	CvsDr	TRINITY_DN16267_c0_g1	13.65094365	-7.610112593	3.05717017	-2.489266927	0.012800682	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1303_c0_g1	138.2078849	-7.18335267	2.586768114	-2.776960421	0.005486986	0.076888617	down_regulated
stress	under	CvsDr	TRINITY_DN1435494_c0_g1	8.434720924	-6.914864159	3.229452978	-2.141187441	0.032258925	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1699202_c0_g1	8.413096491	-6.912908558	3.616544735	-1.911467731	0.0559445	NA	down_regulated
stress	under	CvsDr	TRINITY_DN9247_c5_g1	7.481903779	-6.743000157	3.184865494	-2.117200921	0.034242799	NA	down_regulated
stress	under	CvsDr	TRINITY_DN80219_c0_g1	7.316775156	-6.710174374	3.200231435	-2.096777846	0.036013246	NA	down_regulated
stress	under	CvsDr	TRINITY_DN2940_c0_g1	6.991961297	-6.646086967	3.263411153	-2.036546011	0.041695558	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1303_c1_g1	6.361236317	-6.508229402	3.243911976	-2.006290384	0.044825275	NA	down_regulated
stress	under	CvsDr	TRINITY_DN85_c0_g4	352.0758734	-6.401627663	3.234090119	-1.979421546	0.047768565	0.236278068	down_regulated
stress	under	CvsDr	TRINITY_DN164716_c0_g1	5.4327653	-6.280371115	3.308837186	-1.898059881	0.057688194	NA	down_regulated
stress	under	CvsDr	TRINITY_DN87655_c0_g1	13.99684381	-6.270551939	3.041126248	-2.061917667	0.039215575	NA	down_regulated
stress	under	CvsDr	TRINITY_DN21835_c1_g1	38.27676914	-5.127207658	2.572886107	-1.99278454	0.046285045	0.236241366	down_regulated
stress	under	CvsDxD	TRINITY_DN11881_c1_g1	6.206463833	-6.340600409	3.152005283	-2.011608433	0.04426123	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN49379_c2_g2	5.19891918	-6.083611486	3.230374499	-1.883252697	0.059666131	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN6552_c0_g1	16.05463101	-5.14345254	2.623760297	-1.960336295	0.049956496	NA	down_regulated
stress	under	CvsDi	TRINITY_DN24680_c0_g1	26.79655125	4.339927724	2.195310793	1.976908115	0.048052017	0.28336129	up_regulated
stress	under	CvsDi	TRINITY_DN41875_c0_g1	52.66580299	4.517510231	2.316132018	1.950454549	0.051121966	0.292598074	up_regulated
stress	under	CvsDi	TRINITY_DN253328_c0_g1	16.08923488	4.692683831	2.358962744	1.989299679	0.046668135	NA	up_regulated
stress	under	CvsDi	TRINITY_DN843_c6_g1	106.615102	4.756368863	2.170374421	2.191496922	0.028415852	0.197490173	up_regulated
stress	under	CvsDi	TRINITY_DN1303_c2_g1	29.97454907	5.484982923	2.545984014	2.1543666	0.031211434	0.208004966	up_regulated
stress	under	CvsDi	TRINITY_DN14454_c1_g1	17.21488525	5.748756245	2.731308937	2.10476236	0.035311999	0.227540233	up_regulated
stress	under	CvsDi	TRINITY_DN56437_c0_g1	6.33920456	5.850551617	3.081344161	1.898701122	0.057603784	NA	up_regulated
stress	under	CvsDi	TRINITY_DN20844_c0_g1	19.53681411	5.938200116	2.653877906	2.237555881	0.025250031	0.186123337	up_regulated
stress	under	CvsDi	TRINITY_DN5345_c0_g1	6.977259537	5.988831679	3.052979748	1.961634918	0.049805004	NA	up_regulated
stress	under	CvsDi	TRINITY_DN87808_c0_g1	7.256750796	6.045912138	3.063746055	1.973372476	0.048453141	NA	up_regulated
stress	under	CvsDi	TRINITY_DN156821_c0_g1	7.317114406	6.057675332	3.024800028	2.002669689	0.045212754	NA	up_regulated
stress	under	CvsDi	TRINITY_DN89139_c1_g1	22.14970719	6.103303141	3.100928095	1.968218209	0.049042936	0.287462511	up_regulated
stress	under	CvsDi	TRINITY_DN10265_c0_g3	8.276005843	6.234967866	3.067946937	2.032293255	0.042123977	NA	up_regulated
stress	under	CvsDi	TRINITY_DN2712_c0_g2	24.55101807	6.254174918	3.095164046	2.02062793	0.043318295	0.263429384	up_regulated
stress	under	CvsDi	TRINITY_DN77526_c0_g2	8.46102418	6.266914433	3.023631446	2.072644945	0.038205335	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1180_c0_g1	9.355934062	6.412259035	2.943212249	2.17866008	0.029356929	NA	up_regulated
stress	under	CvsDi	TRINITY_DN988601_c0_g1	9.412679727	6.42122596	3.142915936	2.043079131	0.041044605	NA	up_regulated
stress	under	CvsDi	TRINITY_DN49379_c2_g1	9.435316081	6.424678435	3.035614103	2.116434506	0.03430787	NA	up_regulated
stress	under	CvsDi	TRINITY_DN74872_c0_g1	9.642970772	6.456071758	3.014878582	2.14140357	0.032241507	NA	up_regulated
stress	under	CvsDi	TRINITY_DN53745_c1_g1	9.805352756	6.48012345	3.411154303	1.899686403	0.057474286	NA	up_regulated
stress	under	CvsDi	TRINITY_DN38523_c2_g1	10.005462	6.509265734	3.406290242	1.91095452	0.056010426	NA	up_regulated
stress	under	CvsDi	TRINITY_DN36319_c0_g1	10.20557124	6.537830947	3.401631591	1.921969141	0.054609641	NA	up_regulated
stress	under	CvsDi	TRINITY_DN34677_c0_g1	29.95396755	6.545024675	3.08718092	2.120065148	0.034000552	0.222030448	up_regulated
stress	under	CvsDi	TRINITY_DN26604_c0_g2	30.58447707	6.588576772	2.769612072	2.378880725	0.017365294	0.16091839	up_regulated
stress	under	CvsDi	TRINITY_DN2221_c2_g3	10.71142603	6.60756249	2.925355454	2.258721237	0.023900731	NA	up_regulated
stress	under	CvsDi	TRINITY_DN63454_c2_g1	10.80589896	6.620281973	3.388774564	1.953591733	0.050749519	NA	up_regulated
stress	under	CvsDi	TRINITY_DN484720_c0_g1	10.82098986	6.622353252	3.061367905	2.163200719	0.030525745	NA	up_regulated
stress	under	CvsDi	TRINITY_DN15543_c1_g2	11.40622668	6.698274266	3.377390238	1.983269268	0.04733736	NA	up_regulated
stress	under	CvsDi	TRINITY_DN32378_c0_g1	11.40622668	6.698274266	3.377390238	1.983269268	0.04733736	NA	up_regulated
stress	under	CvsDi	TRINITY_DN39434_c3_g1	11.80644515	6.748021014	3.370507798	2.002078446	0.045276295	NA	up_regulated
stress	under	CvsDi	TRINITY_DN33686_c0_g1	12.60688211	6.842646134	3.358196286	2.037595647	0.041590388	NA	up_regulated
stress	under	CvsDi	TRINITY_DN167772_c0_g1	12.76140909	6.859901637	2.891568261	2.372381012	0.017673855	NA	up_regulated
stress	under	CvsDi	TRINITY_DN17669_c0_g2	13.41486453	6.932305197	3.082584628	2.248861275	0.024521323	NA	up_regulated
stress	under	CvsDi	TRINITY_DN14958_c0_g1	14.43050163	7.037586832	2.98812767	2.35518278	0.018513597	NA	up_regulated
stress	under	CvsDi	TRINITY_DN85021_c0_g1	15.40841147	7.13211971	3.326359564	2.144121696	0.032023137	NA	up_regulated
stress	under	CvsDi	TRINITY_DN21835_c0_g1	17.00928539	7.274709388	3.313578768	2.195423709	0.028133218	0.197490173	up_regulated
stress	under	CvsDi	TRINITY_DN12509_c0_g2	17.82481325	7.34231541	2.997940295	2.449119958	0.014320575	0.148233181	up_regulated
stress	under	CvsDi	TRINITY_DN35732_c0_g2	23.03519894	7.712232955	2.950000544	2.614315774	0.008940636	0.119167658	up_regulated
stress	under	CvsDi	TRINITY_DN4062_c0_g1	28.01529359	7.994540312	3.272137138	2.443216765	0.014556991	0.148452241	up_regulated
stress	under	CvsDi	TRINITY_DN7105_c1_g1	31.23213233	8.151383477	2.970116767	2.744465661	0.00606095	0.103461472	up_regulated
stress	under	CvsDi	TRINITY_DN8703_c0_g1	84.24599	22.32872197	3.263664441	6.841610826	7.83E-12	5.44E-10	up_regulated
stress	under	CvsDr	TRINITY_DN4017_c6_g1	36.64366982	5.841128087	2.615815328	2.233004763	0.02554863	0.163733396	up_regulated
stress	under	CvsDr	TRINITY_DN4257_c0_g2	8.315399346	6.169662313	3.155272294	1.955350201	0.05054175	NA	up_regulated
stress	under	CvsDr	TRINITY_DN14958_c0_g1	11.61231597	6.654183287	3.09412016	2.15058981	0.03150859	NA	up_regulated
stress	under	CvsDr	TRINITY_DN26604_c0_g2	35.67869024	6.787963264	2.891966008	2.347179477	0.01891614	0.138031639	up_regulated
stress	under	CvsDr	TRINITY_DN69016_c0_g2	13.61867286	6.880658853	3.583578508	1.920052494	0.054851269	NA	up_regulated
stress	under	CvsDr	TRINITY_DN29_c0_g3	14.83462579	7.004189407	3.574297269	1.959599015	0.050042677	NA	up_regulated
stress	under	CvsDr	TRINITY_DN19321_c0_g1	17.34645766	7.233233191	3.529886195	2.049140621	0.040448366	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1180_c0_g1	17.47985189	7.240891474	3.120422493	2.320484322	0.020314692	NA	up_regulated
stress	under	CvsDr	TRINITY_DN8703_c0_g1	20.90470538	7.502160769	3.517421268	2.132858193	0.032936361	NA	up_regulated
stress	under	CvsDr	TRINITY_DN2831_c0_g3	22.37353398	7.597587456	3.544008768	2.143783482	0.032050239	NA	up_regulated
stress	under	CvsDr	TRINITY_DN30172_c8_g1	25.50077536	7.788640247	3.508449273	2.219966612	0.026421034	0.163947225	up_regulated
stress	under	CvsDr	TRINITY_DN12274_c0_g1	32.02422952	8.117041382	3.502643837	2.317404155	0.020481726	0.141046167	up_regulated
stress	under	CvsDr	TRINITY_DN34347_c2_g2	33.95161371	8.201307814	3.501810055	2.342019608	0.019179707	0.138582787	up_regulated
stress	under	CvsDxD	TRINITY_DN2831_c0_g2	131.7550097	4.489824361	2.336165538	1.921877662	0.054621153	0.232907052	up_regulated
stress	under	CvsDxD	TRINITY_DN6287_c0_g1	21.77835414	4.518593132	2.369664947	1.906848957	0.05654015	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN69787_c0_g1	168.500177	4.703785883	2.141708771	2.196277078	0.028072117	0.139462498	up_regulated
stress	under	CvsDxD	TRINITY_DN1096_c0_g1	187.5833932	4.747680858	2.166795694	2.191106836	0.028444062	0.140188592	up_regulated
stress	under	CvsDxD	TRINITY_DN9125_c0_g1	43.78609675	4.817447007	2.35880209	2.042327768	0.041119029	0.185794821	up_regulated
stress	under	CvsDxD	TRINITY_DN861_c0_g1	182.7439083	5.040600775	2.193406319	2.298069779	0.021557818	0.122107342	up_regulated
stress	under	CvsDxD	TRINITY_DN26096_c0_g1	39.90309749	5.235105507	2.484316842	2.10726161	0.035094909	0.16630566	up_regulated
stress	under	CvsDxD	TRINITY_DN45675_c0_g1	20.63018412	5.260986485	2.597350862	2.025520141	0.042813982	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN7347_c0_g1	201.1203496	5.523059634	2.282580248	2.419656281	0.015535184	0.099995162	up_regulated
stress	under	CvsDxD	TRINITY_DN774_c0_g2	354.2775994	5.586174511	2.313669058	2.414422448	0.015760178	0.100520797	up_regulated
stress	under	CvsDxD	TRINITY_DN4017_c6_g1	31.90693926	5.745866228	2.548344627	2.254744577	0.024149366	0.129729499	up_regulated
stress	under	CvsDxD	TRINITY_DN37896_c0_g2	6.896325709	6.011931571	3.116564337	1.929025337	0.053727717	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN26621_c0_g1	7.015230673	6.037464216	3.191421027	1.891779294	0.058520393	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN252739_c0_g1	7.816809325	6.193394466	3.147839978	1.967506134	0.049124889	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN730_c0_g3	87.24161196	6.288786539	2.323202837	2.706946823	0.006790513	0.067815785	up_regulated
stress	under	CvsDxD	TRINITY_DN29191_c1_g1	8.485736512	6.311920802	3.237002247	1.949927841	0.05118472	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN6046_c0_g3	9.16153695	6.422295153	3.118751654	2.059251863	0.039470117	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN65988_c0_g1	9.421170016	6.460520218	3.076424886	2.100009088	0.035728042	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN89139_c1_g3	27.83926378	6.468843847	2.971467532	2.176986212	0.029481596	0.14436472	up_regulated
stress	under	CvsDxD	TRINITY_DN17349_c4_g1	9.545744843	6.481534289	3.112166129	2.082644056	0.03728368	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN26659_c1_g1	9.878576892	6.528707514	3.223117726	2.025587667	0.042807056	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN18029_c1_g1	9.89678987	6.533650795	3.127918282	2.088817612	0.036724144	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN62845_c0_g2	10.36227338	6.597879645	3.503993911	1.882959792	0.059705817	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1287858_c0_g1	10.56571706	6.627797747	3.490708604	1.898696941	0.057604334	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN2245_c1_g1	12.12437177	6.824558736	3.042346604	2.243189099	0.024884623	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN5384_c2_g2	13.06547514	6.932472926	3.470535658	1.99752246	0.045768457	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN10265_c0_g3	14.59989993	7.094127055	3.44793093	2.057502659	0.039637899	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN44394_c0_g2	15.36831572	7.168094892	3.442808287	2.082048808	0.037338012	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN27766_c0_g1	16.31062247	7.253215086	2.926999276	2.478037882	0.013210712	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN774_c0_g1	164.6885308	7.307768434	2.455419272	2.976179473	0.00291864	0.041797128	up_regulated
stress	under	CvsDxD	TRINITY_DN19634_c1_g1	98.35410775	7.543083487	2.615057912	2.884480474	0.003920601	0.051305792	up_regulated
stress	under	CvsDxD	TRINITY_DN1649245_c0_g1	23.2882238	7.766580484	2.977617781	2.608320159	0.009098781	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1557528_c0_g1	24.32881577	7.829710265	3.42256078	2.287676032	0.022156394	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN123572_c0_g1	25.45514983	7.895019338	3.420855261	2.307908033	0.021004252	0.12077445	up_regulated
stress	under	CvsDxD	TRINITY_DN36234_c0_g2	27.70781796	8.017385859	3.418166603	2.345522261	0.019000444	0.116301104	up_regulated
stress	under	CvsDxD	TRINITY_DN2831_c0_g3	28.05404944	8.036158752	3.007783651	2.671787497	0.007544841	0.070697955	up_regulated
stress	under	CvsDxD	TRINITY_DN153188_c0_g1	28.49565011	8.057826997	2.943197074	2.737780309	0.006185538	0.064459963	up_regulated
stress	under	CvsDxD	TRINITY_DN57237_c4_g1	67.99741457	9.312765168	3.163321818	2.943982846	0.00324018	0.044714478	up_regulated
stress	under	CvsDxD	TRINITY_DN29_c0_g3	117.6901304	10.10449926	2.862337047	3.530157036	0.000415313	0.010869746	up_regulated
stress	under	CvsDxD	TRINITY_DN1180_c0_g1	118.7614784	10.1175849	2.919263769	3.465800182	0.000528656	0.012303439	up_regulated
   ")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


HSP90_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

HSP90_reg

HSP90_reg_final <- HSP90_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

HSP90_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

HSP90_reg_final_plot <- HSP90_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                               panel.background = element_rect(fill = "white"), 
                                                                                               panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                               axis.title.x = element_text(color="black", vjust=1),
                                                                                               axis.title.y = element_text(color="black" , vjust=1)) 






HSP90_reg_final_plot

HSP90_reg_final_plot_facet <- HSP90_reg_final_plot + facet_grid( ~ location, scales='free_y') #scales='fixed' makes that all y-axes are the same

HSP90_reg_final_plot_facet

jpeg("HSP90_regulation_FACET.jpeg",height=6,width=10,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(HSP90_reg_final_plot_facet)
dev.off()



################ multicopper oxidase 


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
pathogenicity	inter	CvsDi	TRINITY_DN272308_c0_g1	8.331018555	-6.495893412	3.281036695	-1.979829553	0.047722684	0.922241767	down_regulated
pathogenicity	inter	CvsDi	TRINITY_DN489606_c0_g1	31.48351208	-8.414088873	3.619287419	-2.324791567	0.020083108	0.922241767	down_regulated
pathogenicity	inter	CvsDi	TRINITY_DN60385_c0_g1	12.72065102	-7.106274056	3.698842472	-1.921215653	0.054704526	0.922241767	down_regulated
pathogenicity	inter	CvsDr	TRINITY_DN489606_c0_g1	30.71572017	-8.272514627	3.713506905	-2.227682576	0.02590169	0.908093125	down_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN489606_c0_g1	26.35234141	-7.852050846	3.978745308	-1.973499241	0.048438711	0.935664183	down_regulated
pathogenicity	inter	CvsDi	TRINITY_DN90163_c0_g1	28.44800137	-8.267564648	3.626562347	-2.279724945	0.022624007	0.922241767	down_regulated
pathogenicity	inter	CvsDr	TRINITY_DN12594_c0_g1	163.9984872	-5.235477196	2.520546482	-2.077119876	0.037790497	0.908093125	down_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN573_c0_g1	2080.39405	27.0559407	4.113625948	6.57715141	4.80E-11	4.63E-08	up_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN703318_c0_g1	3552.365812	28.11129911	4.139043116	6.791738652	1.11E-11	2.14E-08	up_regulated
pathogenicity	inter	CvsDr	TRINITY_DN46989_c0_g1	11.23563	7.2021914	3.758638513	1.916170277	0.055343425	0.908093125	up_regulated
pathogenicity	inter	CvsDr	TRINITY_DN161346_c0_g1	11.56665154	7.245000746	3.288543526	2.203103194	0.027587476	0.908093125	up_regulated
pathogenicity	inter	CvsDr	TRINITY_DN336788_c0_g1	15.1681005	7.635262394	3.728418214	2.047855674	0.040574143	0.908093125	up_regulated
pathogenicity	inter	CvsDxD	TRINITY_DN1309481_c0_g1	16.51965816	7.905073683	3.971286423	1.990557427	0.046529565	0.935664183	up_regulated
pathogenicity	inter	CvsDi	TRINITY_DN236959_c0_g1	13.17594609	7.321560971	3.677294272	1.991018512	0.046478852	0.922241767	up_regulated
pathogenicity	inter	CvsDi	TRINITY_DN175061_c0_g1	17.34595341	7.716668201	3.126849478	2.467873256	0.013591844	0.922241767	up_regulated
pathogenicity	inter	CvsDr	TRINITY_DN63077_c0_g1	14.98543899	6.163036194	3.028048992	2.035315878	0.041819099	0.908093125	up_regulated
pathogenicity	inter	CvsDr	TRINITY_DN17739_c0_g1	33.8677991	6.358301035	2.813969737	2.259548478	0.023849288	0.908093125	up_regulated
pathogenicity	inter	CvsDr	TRINITY_DN390315_c0_g1	7.281739462	6.576158968	3.495109213	1.881531754	0.05989962	0.908093125	up_regulated
pathogenicity	under	CvsDr	TRINITY_DN274300_c0_g1	8.952857532	-6.98885114	3.340973843	-2.091860478	0.036450998	0.962938997	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN274300_c0_g1	7.490268348	-6.67291218	3.230515955	-2.06558713	0.038867481	NA	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN20609_c0_g1	170.8692578	-11.24329304	3.186876958	-3.527997218	0.000418717	0.609441949	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN85730_c0_g1	10.56181395	-7.227281113	3.370311516	-2.144395579	0.032001204	0.962938997	down_regulated
pathogenicity	under	CvsDi	TRINITY_DN85730_c0_g2	10.56181395	0	3.370311516	-2.144395579	0.032001204	0.962938997	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN61429_c0_g1	40.99286317	6.992959793	3.486839932	2.005529342	0.044906487	0.962938997	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN1815177_c0_g1	20.56133201	7.492396183	3.166052392	2.3664789	0.017958198	0.979901061	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN22846_c0_g1	24.7660196	5.24134204	2.748446514	1.907019843	0.056518018	0.24578766	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN178883_c0_g2	10.59879275	6.634168927	3.222642203	2.058611695	0.039531452	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN331557_c0_g1	11.57356589	6.756616452	3.323892662	2.03274207	0.042078589	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN102333_c0_g2	16.40270926	7.263140516	3.098712928	2.343921714	0.019082176	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN22778_c0_g1	18.42503618	7.430816388	3.568085913	2.082577766	0.037289728	NA	up_regulated
pathogenicity	under	CvsDxD	TRINITY_DN963324_c0_g1	103.6524933	22.65802965	3.60653927	6.282485218	3.33E-10	1.32E-08	up_regulated
   ")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


multicop_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

multicop_reg

multicop_reg_final <- multicop_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

multicop_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

multicop_reg_final_plot <- multicop_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                                     panel.background = element_rect(fill = "white"), 
                                                                                                     panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                                     axis.title.x = element_text(color="black", vjust=1),
                                                                                                     axis.title.y = element_text(color="black" , vjust=1)) 






multicop_reg_final_plot

multicop_reg_final_plot_facet <- multicop_reg_final_plot + facet_grid( ~ location, scales='free_y') #scales='fixed' makes that all y-axes are the same
multicop_reg_final_plot_facet

jpeg("multicop_regulation_FACET.jpeg",height=6,width=10,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(multicop_reg_final_plot_facet)
dev.off()


################ phage encoded virulence factor 


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation	
pathogenicity	inter	CvsDi	TRINITY_DN1341961_c0_g1	114.8605372	-23.68761099	3.628972978	-6.527359431	6.69E-11	9.30E-08	down_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN1324060_c0_g1	90.54223818	-23.25197269	3.622127163	-6.419424731	1.37E-10	9.30E-08	down_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN1341961_c0_g1	112.0594205	-23.54015608	3.736940142	-6.299313125	2.99E-10	4.21E-07	down_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN1324060_c0_g1	88.33417455	-23.22001062	3.728809123	-6.227192076	4.75E-10	4.21E-07	down_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN2076_c0_g1	126.7983656	-10.31805494	3.292309747	-3.133986696	0.001724487	0.611503001	down_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN1341961_c0_g1	96.14061108	-23.15112627	4.004279614	-5.781595819	7.40E-09	2.38E-06	down_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN2076_c0_g1	108.9729892	-9.899955411	3.527309107	-2.806659442	0.005005813	0.935664183	down_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN1324060_c0_g1	75.78569909	-9.375948921	3.996154429	-2.346242891	0.018963745	0.935664183	down_regulated	
pathogenicity	inter	CvsDi	TRINITY_DN1324060_c0_g2	75.78569909	0	3.996154429	-2.346242891	0.018963745	0.935664183	up_regulated	
pathogenicity	inter	CvsDr	TRINITY_DN1324060_c0_g3	75.78569909	0	3.996154429	-2.346242891	0.018963745	0.935664183	up_regulated	
pathogenicity	inter	CvsDxD	TRINITY_DN1324060_c0_g4	75.78569909	0	3.996154429	-2.346242891	0.018963745	0.935664183	up_regulated	
pathogenicity	under	CvsDi	TRINITY_DN50605_c0_g2	29.69039921	-7.268407411	3.256960364	-2.231653627	0.025637864	0.979901061	down_regulated
pathogenicity	under	CvsDr	TRINITY_DN50605_c0_g2	33.95004363	-8.91185527	3.690899842	-2.414548118	0.015754742	0.962938997	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN50605_c0_g2	27.29191466	-8.536817222	3.540596644	-2.41112391	0.015903444	0.135088704	down_regulated
pathogenicity	under	CvsDxD	TRINITY_DN23423_c1_g1	16.26397435	7.248308015	3.5979745	2.014552359	0.043951574	NA	up_regulated
pathogenicity	under	CvsDi	TRINITY_DN23423_c1_g2	16.26397435	0	3.5979745	2.014552359	0.043951574	NA	up_regulated
pathogenicity	under	CvsDr	TRINITY_DN23423_c1_g3	16.26397435	0	3.5979745	2.014552359	0.043951574	NA	up_regulated
   ")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


phage_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

phage_reg

phage_reg_final <- phage_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

phage_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

phage_reg_final_plot <- phage_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                               panel.background = element_rect(fill = "white"), 
                                                                                               panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                               axis.title.x = element_text(color="black", vjust=1),
                                                                                               axis.title.y = element_text(color="black" , vjust=1)) 






phage_reg_final_plot

phage_reg_final_plot_facet <- phage_reg_final_plot + facet_grid( ~ location, scales='free_y') #scales='fixed' makes that all y-axes are the same

phage_reg_final_plot_facet

jpeg("phage_regulation_FACET.jpeg",height=6,width=10,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(phage_reg_final_plot_facet)
dev.off()


################ stress induced bacterial acidophilic repeat motif 


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	inter	CvsDr	TRINITY_DN7618_c0_g2	39.9786291	-8.639979882	3.57570401	-2.416301757	0.015679061	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN1309107_c0_g1	39.12400629	-8.608838255	3.575292063	-2.407869932	0.016045897	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN566591_c0_g1	22.76707711	-7.827757599	3.592534017	-2.178895888	0.029339403	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN485541_c0_g1	22.10395836	-7.785114698	3.594292559	-2.165965783	0.030313802	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN27960_c0_g1	97.2094991	-6.222110295	3.213534719	-1.936220031	0.052840758	0.293673665	down_regulated
stress	inter	CvsDi	TRINITY_DN7618_c0_g2	41.4809314	-8.984522229	3.485146942	-2.577946462	0.00993894	0.097685577	down_regulated
stress	inter	CvsDi	TRINITY_DN566591_c0_g1	26.79610512	-8.353591778	3.509833204	-2.380053778	0.017310112	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN485541_c0_g1	26.01563604	-8.310938868	3.511752183	-2.366607447	0.017951963	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN351352_c0_g1	22.7102306	-8.115589633	3.514876696	-2.308925842	0.020947697	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN27960_c0_g1	114.7735982	-5.92239275	3.14048556	-1.885820723	0.059319117	0.385014649	down_regulated
stress	inter	CvsDxD	TRINITY_DN1309107_c0_g1	39.11302411	-8.386364511	3.897064702	-2.151969534	0.031399754	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN7618_c0_g2	33.78384916	-8.175462764	3.88847163	-2.10248744	0.035510599	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN566591_c0_g1	22.76068635	-7.605141914	3.904793987	-1.947642293	0.051457779	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN485541_c0_g1	22.09775373	-7.562488912	3.905845935	-1.936197443	0.052843524	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN747053_c0_g1	10.15688274	6.956220375	3.680658491	1.889939094	0.058766106	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN67032_c0_g1	11.57394393	7.146470066	3.285216487	2.175342201	0.029604482	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN12263_c0_g3	23.33387584	8.157118034	3.046128766	2.677863826	0.007409333	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN711967_c0_g1	19.12381264	7.725562512	3.543668133	2.180103278	0.029249807	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN12263_c0_g3	149.2382999	10.68998572	3.251630081	3.287577446	0.001010534	0.021726479	up_regulated
stress	inter	CvsDxD	TRINITY_DN105079_c0_g1	9.435590265	6.972032102	3.552567713	1.962533206	0.049700439	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN36780_c0_g1	17.63524961	7.874504693	3.890045121	2.024270786	0.042942298	NA	up_regulated
stress	under	CvsDi	TRINITY_DN2117_c1_g1	17.90340205	-7.897780133	3.293604616	-2.397913853	0.016488743	0.157289673	down_regulated
stress	under	CvsDi	TRINITY_DN188268_c0_g1	8.446428976	-6.818075505	3.422310011	-1.992243684	0.046344327	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1884_c2_g1	28.87197008	-8.691331623	3.207216748	-2.709929607	0.006729749	0.086631267	down_regulated
stress	under	CvsDr	TRINITY_DN486859_c0_g1	20.13133803	-8.171151729	3.502683136	-2.332826411	0.019657258	NA	down_regulated
stress	under	CvsDr	TRINITY_DN2117_c1_g1	17.35938433	-7.956579407	3.518519145	-2.261343218	0.023738012	NA	down_regulated
stress	under	CvsDr	TRINITY_DN188268_c0_g1	9.614967418	-7.105457238	3.589330097	-1.979605399	0.047747886	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN2117_c1_g1	17.76753788	-7.857459742	3.424688813	-2.294357289	0.021769984	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN9117_c0_g1	57.30051175	-4.681386509	2.464769689	-1.899320058	0.057522407	0.235997335	down_regulated
stress	under	CvsDi	TRINITY_DN378_c0_g1	453.1525997	4.326035006	2.234279527	1.936210288	0.052841951	0.298925688	up_regulated
stress	under	CvsDi	TRINITY_DN172039_c0_g1	7.619242019	6.116316148	3.139061246	1.948453907	0.051360675	NA	up_regulated
stress	under	CvsDi	TRINITY_DN11833_c0_g1	79.22954906	6.303939659	2.467417848	2.55487317	0.01062265	0.132698631	up_regulated
stress	under	CvsDi	TRINITY_DN512091_c0_g1	10.08453445	6.520230922	2.948807268	2.21114177	0.027026021	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1767260_c0_g1	15.82341129	7.170217701	2.838713729	2.52586854	0.011541266	NA	up_regulated
stress	under	CvsDi	TRINITY_DN89700_c0_g1	15.91395671	7.178383684	2.890113012	2.48377266	0.012999877	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1575170_c0_g1	22.81245335	7.698169932	3.285078453	2.34337476	0.019110177	0.164412176	up_regulated
stress	under	CvsDr	TRINITY_DN26743_c0_g1	98.09311748	6.634918845	3.166962645	2.095041713	0.036167285	0.204115944	up_regulated
stress	under	CvsDr	TRINITY_DN11210_c0_g2	16.75341637	7.183099647	3.532704296	2.03331472	0.042020737	NA	up_regulated
stress	under	CvsDr	TRINITY_DN4502_c0_g1	48.23608114	7.221189215	3.333009819	2.166567039	0.030267883	0.175649053	up_regulated
stress	under	CvsDr	TRINITY_DN24552_c0_g2	18.53254023	7.328556149	3.524972747	2.079039095	0.037613757	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN4502_c0_g1	24.8686994	6.313318136	2.763546174	2.284498879	0.022342228	0.124689348	up_regulated
stress	under	CvsDxD	TRINITY_DN331980_c0_g2	9.996278469	6.547941065	3.076819566	2.12815244	0.033324447	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1828000_c0_g1	30.07045584	6.569664231	3.214504195	2.043756621	0.040977597	0.185794821	up_regulated
stress	under	CvsDxD	TRINITY_DN947180_c0_g1	13.06547514	6.932472926	3.470535658	1.99752246	0.045768457	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN54397_c0_g2	43.5864646	7.110327517	3.214298739	2.212092931	0.026960244	0.138018561	up_regulated
stress	under	CvsDxD	TRINITY_DN139075_c0_g1	16.49464978	7.270220007	3.038039762	2.393062822	0.01670838	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN48852_c0_g1	17.57081139	7.360074233	3.441273353	2.138764776	0.032454723	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN679476_c0_g1	26.5814839	7.957499755	3.41940223	2.327161071	0.019956694	0.11833696	up_regulated
stress	under	CvsDxD	TRINITY_DN1877249_c0_g1	26.80675071	7.969677688	3.419138725	2.330902115	0.019758522	0.11833696	up_regulated
stress	under	CvsDxD	TRINITY_DN948028_c0_g1	166.2415805	7.98382832	3.138989655	2.543438876	0.010976726	0.088322486	up_regulated
stress	under	CvsDxD	TRINITY_DN5365_c0_g1	66.22844293	9.274746408	3.418529173	2.713080959	0.006666082	0.067460748	up_regulated
stress	under	CvsDxD	TRINITY_DN1551842_c0_g1	200.8481127	10.8755535	3.122754634	3.482679486	0.000496422	0.01215434	up_regulated
stress	under	CvsDxD	TRINITY_DN49322_c0_g1	102.271133	22.64254555	3.43101851	6.599365606	4.13E-11	2.13E-09	up_regulated
stress	under	CvsDxD	TRINITY_DN105788_c0_g1	103.8480007	22.66441406	3.431542282	6.604731108	3.98E-11	2.13E-09	up_regulated
   ")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


stress_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

stress_reg

stress_reg_final <- stress_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

stress_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

stress_reg_final_plot <- stress_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                                 panel.background = element_rect(fill = "white"), 
                                                                                                 panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                                 axis.title.x = element_text(color="black", vjust=1),
                                                                                                 axis.title.y = element_text(color="black" , vjust=1)) 






stress_reg_final_plot

stress_reg_final_plot_facet <- stress_reg_final_plot + facet_grid( ~ location, scales='free_y') #scales='fixed' makes that all y-axes are the same

stress_reg_final_plot_facet

jpeg("stress_regulation_FACET.jpeg",height=6,width=10,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(stress_reg_final_plot_facet)
dev.off()




################ universal stress protein family 


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	inter	CvsDr	TRINITY_DN19054_c0_g1	15.69381043	-7.291023643	3.622908417	-2.012478044	0.044169569	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN80569_c0_g1	23.95316853	-6.398480778	3.003381604	-2.130425507	0.0331365	NA	down_regulated
stress	inter	CvsDi	TRINITY_DN26273_c0_g1	7.760999803	-6.567145452	3.295741799	-1.992615275	0.046303591	NA	down_regulated
stress	inter	CvsDxD	TRINITY_DN26273_c0_g2	7.760999803	0	3.295741799	-1.992615275	0.046303591	NA	down_regulated
stress	inter	CvsDr	TRINITY_DN125600_c0_g1	7.006525183	6.422647002	3.353553028	1.915176813	0.055469958	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN422100_c0_g1	7.1513182	6.44972392	3.356032916	1.921829756	0.054627183	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN1370168_c0_g1	7.64204865	6.546609828	3.259968338	2.008182028	0.044623952	NA	up_regulated
stress	inter	CvsDi	TRINITY_DN615905_c0_g1	58.86017656	9.347882612	3.488596297	2.679554129	0.007372028	0.089270477	up_regulated
stress	inter	CvsDxD	TRINITY_DN538056_c0_g1	11.68644921	7.280789898	3.480102009	2.092119679	0.036427811	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN1295788_c0_g1	12.73656917	7.40494191	3.91659018	1.890660388	0.058669693	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN25140_c0_g1	13.33693866	7.471395412	3.488674534	2.141614341	0.032224529	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN806387_c0_g1	15.34919874	7.674175903	3.89964222	1.967917945	0.04907748	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN615905_c0_g1	31.24244759	8.699957058	3.486611367	2.495247144	0.012586943	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN67427_c0_g2	37.07792751	8.946928388	3.866957419	2.313686813	0.020684908	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN67427_c0_g1	38.0048757	8.982549256	3.866969963	2.322890879	0.020185015	NA	up_regulated
stress	under	CvsDi	TRINITY_DN956523_c0_g1	31.67410866	-8.723083183	3.236219141	-2.695455037	0.007029256	0.112025547	down_regulated
stress	under	CvsDr	TRINITY_DN956523_c0_g1	36.05612782	-9.01176896	3.483910097	-2.586682408	0.009690486	0.099501893	down_regulated
stress	under	CvsDr	TRINITY_DN943950_c0_g1	27.3425636	-8.6127495	3.489026499	-2.468525103	0.013567114	0.113624576	down_regulated
stress	under	CvsDr	TRINITY_DN691487_c0_g1	25.9214284	-8.535856857	3.125049985	-2.731430505	0.006306004	0.082991515	down_regulated
stress	under	CvsDr	TRINITY_DN148458_c0_g2	17.85204851	-7.996952009	3.093819416	-2.58481538	0.009743115	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1294576_c0_g1	13.95685477	-7.642901104	3.11925664	-2.450231573	0.014276436	NA	down_regulated
stress	under	CvsDr	TRINITY_DN167334_c0_g1	10.92510963	-7.289706596	3.162390419	-2.305125436	0.021159549	NA	down_regulated
stress	under	CvsDr	TRINITY_DN47134_c0_g1	9.942502971	-7.153899302	3.279514419	-2.1813898	0.029154597	NA	down_regulated
stress	under	CvsDr	TRINITY_DN1357530_c0_g1	9.281988476	-7.053326148	3.148810988	-2.239996676	0.025091139	NA	down_regulated
stress	under	CvsDr	TRINITY_DN400333_c0_g1	6.637357922	-6.571217731	3.357187862	-1.957357765	0.05030542	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN1294576_c0_g1	12.00351551	-7.295239577	3.041758457	-2.398362552	0.016468556	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN725545_c0_g1	11.83681471	-7.27533308	3.079891361	-2.362204451	0.01816662	NA	down_regulated
stress	under	CvsDxD	TRINITY_DN943950_c0_g1	23.14690766	-6.727068723	3.194301599	-2.105959163	0.0352079	NA	down_regulated
stress	under	CvsDi	TRINITY_DN1598275_c0_g1	48.71078633	5.230164273	2.493814702	2.097254566	0.035971046	0.230262028	up_regulated
stress	under	CvsDi	TRINITY_DN1259300_c0_g1	7.056641556	6.005570516	3.072331595	1.954727324	0.050615263	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1311228_c0_g1	9.605243516	6.450380361	3.416236512	1.88815392	0.05900529	NA	up_regulated
stress	under	CvsDi	TRINITY_DN1557260_c0_g1	12.40677287	6.819565337	3.361107267	2.028963909	0.042461967	NA	up_regulated
stress	under	CvsDr	TRINITY_DN1557260_c0_g2	12.40677287	0	3.361107267	2.028963909	0.042461967	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN947240_c0_g1	12.51424953	6.87123436	2.969597314	2.313860646	0.020675367	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN136480_c0_g1	13.25517231	6.954789296	3.45873192	2.01079166	0.044347467	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN1251170_c0_g1	13.6393802	6.995992086	3.455375598	2.024669067	0.042901357	NA	up_regulated
   ")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


universal_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

universal_reg

universal_reg_final <- universal_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

universal_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

universal_reg_final_plot <- universal_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                                       panel.background = element_rect(fill = "white"), 
                                                                                                       panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                                       axis.title.x = element_text(color="black", vjust=1),
                                                                                                       axis.title.y = element_text(color="black" , vjust=1)) 






universal_reg_final_plot

universal_reg_final_plot_facet <- universal_reg_final_plot + facet_grid( ~ location, scales='free_y') #scales='fixed' makes that all y-axes are the same

universal_reg_final_plot_facet

jpeg("universal_regulation_FACET.jpeg",height=6,width=10,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(universal_reg_final_plot_facet)
dev.off()




################ viral superfamily 1 RNA helicase 


Input = 
  ("
genes	location	treatment	row	baseMean	fold_change	lfcSE	stat	pvalue	padj	regulation
stress	inter	CvsDr	TRINITY_DN3820_c0_g1	2900.260376	-27.97272959	3.740756587	-7.477826729	7.56E-14	2.14E-11	down_regulated
stress	inter	CvsDi	TRINITY_DN3820_c0_g1	3413.511604	-28.47488287	3.654349805	-7.79205177	6.59E-15	2.27E-12	down_regulated
stress	inter	CvsDxD	TRINITY_DN3820_c0_g1	2899.446268	-27.76029911	4.066164192	-6.827146619	8.66E-12	2.49E-09	down_regulated
stress	inter	CvsDi	TRINITY_DN374486_c0_g1	15.02262004	0	3.901346816	1.959103952	0.050100614	NA	up_regulated
stress	inter	CvsDr	TRINITY_DN374486_c0_g1	15.02262004	0	3.901346816	1.959103952	0.050100614	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN374486_c0_g1	15.02262004	7.643143963	3.901346816	1.959103952	0.050100614	NA	up_regulated
stress	inter	CvsDxD	TRINITY_DN419470_c0_g1	15.44913646	7.684060541	3.895928655	1.972330918	0.048571842	NA	up_regulated
stress	under	CvsDi	TRINITY_DN36423_c1_g1	222.7745642	-24.50895715	3.268619509	-7.498259458	6.47E-14	8.52E-12	down_regulated
stress	under	CvsDi	TRINITY_DN22654_c2_g1	169.9843831	-24.48420508	3.257322829	-7.516665176	5.62E-14	8.52E-12	down_regulated
stress	under	CvsDi	TRINITY_DN12396_c0_g1	53.84598472	-9.488268872	3.227683593	-2.939652726	0.003285803	0.077756635	down_regulated
stress	under	CvsDr	TRINITY_DN36423_c1_g1	253.5947657	-25.03677942	3.539145212	-7.074244747	1.50E-12	7.45E-10	down_regulated
stress	under	CvsDr	TRINITY_DN22654_c2_g1	193.5012193	-24.80378717	3.526814848	-7.032914467	2.02E-12	7.45E-10	down_regulated
stress	under	CvsDr	TRINITY_DN12396_c0_g1	62.18497922	-6.075840439	3.106665119	-1.955743605	0.050495365	0.24483608	down_regulated
stress	under	CvsDxD	TRINITY_DN36423_c1_g1	212.5919217	-24.77041642	3.428611582	-7.22462018	5.03E-13	7.63E-11	down_regulated
stress	under	CvsDxD	TRINITY_DN22654_c2_g1	162.2146891	-24.40188101	3.416475834	-7.142412883	9.17E-13	1.16E-10	down_regulated
stress	under	CvsDxD	TRINITY_DN12396_c0_g1	51.6100441	-7.89095053	3.192019037	-2.472087553	0.013432661	0.094401759	down_regulated
stress	under	CvsDr	TRINITY_DN1777829_c0_g1	60.79764669	9.040557137	3.533486073	2.558537645	0.010511344	0.099505535	up_regulated
stress	under	CvsDi	TRINITY_DN1777829_c0_g2	60.79764669	0	3.533486073	2.558537645	0.010511344	0.099505535	up_regulated
stress	under	CvsDxD	TRINITY_DN432385_c0_g1	24.13550844	7.818347376	2.90303628	2.693162132	0.007077784	NA	up_regulated
stress	under	CvsDxD	TRINITY_DN55261_c5_g1	43.25122804	8.659960037	3.413065598	2.537296688	0.011171224	0.088322486	up_regulated
stress	under	CvsDxD	TRINITY_DN1757990_c0_g1	96.41419583	22.55547818	3.429048777	6.577765336	4.78E-11	2.13E-09	up_regulated
   ")

Data = read.table(textConnection(Input),header=TRUE)


library(ggplot2)

library(RColorBrewer)

cbbPalette <- c("#7E7E7E", "black")


viral_reg <- ggplot(Data, aes (treatment, fold_change, fill = regulation)) +
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitterdodge(jitter.width = 0.01)) +
  ylim(-30,30) +
  # geom_boxplot() + 
  # geom_point() + 
  # geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  xlab("") +
  ylab("Fold change expression 
  in response to treatment 
  (log2)") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'))

viral_reg

viral_reg_final <- viral_reg + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        axis.title.x = element_text(color="black", vjust=1),
        axis.title.y = element_text(color="black" , vjust=1)) 

viral_reg_final + scale_fill_brewer(palette="Dark2")+ theme_classic()

viral_reg_final_plot <- viral_reg_final + scale_fill_manual(values = c("blue", "red")) + theme(panel.grid = element_blank(), 
                                                                                               panel.background = element_rect(fill = "white"), 
                                                                                               panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                                                                               axis.title.x = element_text(color="black", vjust=1),
                                                                                               axis.title.y = element_text(color="black" , vjust=1)) 






viral_reg_final_plot


viral_reg_final_plot_facet <- viral_reg_final_plot + facet_grid( ~ location, scales='free_y') #scales='fixed' makes that all y-axes are the same

viral_reg_final_plot_facet

jpeg("viral_regulation_FACET.jpeg",height=6,width=10,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(viral_reg_final_plot_facet)
dev.off()



