# Arquivo para testes
devtools::load_all()
# bal_desp <- parse_bal_desp("C:/Users/Everton/OneDrive/Prefeitura/2020/PAD/2020-10/cm/MES10/BAL_DESP.TXT")
# bal_desp

empenho <- parse_empenho("C:/Users/Everton/OneDrive/Prefeitura/2020/PAD/2020-10/cm/MES10/EMPENHO.TXT")
empenho
write.csv2(empenho, 'empenho.csv')
