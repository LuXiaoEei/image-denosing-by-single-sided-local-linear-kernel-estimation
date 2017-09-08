# image denosing by single sided local linear kernel estimation 

这个包可以用来实现单边局部线性核估计，用来进行图像去燥，主要是使用R来实现Irene Gijbels, Alexandre Lambert, Peihua Qiu等人在2006年文章里提出的算法的《Edge-Preserving Image Denosing and Estimation of Discontinous Surface》.


其中的introduction.pdf有比较详细的说明，如果对更加具体的推导什么的感兴趣的可以查阅原文，理应可以取多种核函数，实际中这个包里就只提到了一个.

其中还提供了四个demo，其中三个是文章中的三个模拟的例子，‘*_cv’  那个例子主要是考虑到参数的选择问题，实现了留一交叉验证思想下的最小残差平方和参数选择


由于本人水平有限，肯定诸多不足的地方，欢迎指正.


