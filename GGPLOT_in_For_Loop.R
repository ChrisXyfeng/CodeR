for (i in vars) {
    print(ggplot(data=dTrain,
                 aes_string(i))+
        geom_density(aes(color=as.factor(TARGET),
                         linetype=as.factor(TARGET))))
}
