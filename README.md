# image denosing by single sided local linear kernel estimati

这个包可以用来实现单边局部线性核估计，用来进行图像去燥，主要是使用R来实现Irene Gijbels, Alexandre Lambert, Peihua Qiu等人在2006年文章里提出的算法的《Edge-Preserving Image Denosing and Estimation of Discontinous Surface》.


其中的introduction.pdf有比较详细的说明，如果对更加具体的推导什么的感兴趣的可以查阅原文，理应可以取多种核函数，实际中这个包里就只提到了一个.


其中还提供了四个demo，其中三个是文章中的三个模拟的例子，`*_cv`那个例子主要是考虑到参数的选择问题，实现了留一交叉验证思想下的最小残差平方和参数选择.


其中有一点需要说明，当时在实现的时候，刚刚接触了`data.table`包，想着不用data.frame了，结果发现`data.table`的一些表述在循环里无法实现我想实现的功能（当然这是我水平的问题，轻拍~），所以就出现了代码里有点混乱的情况，特别是`dplyr`和`data.table`的冲突.


由于本人水平有限，肯定诸多不足的地方，欢迎指正.


