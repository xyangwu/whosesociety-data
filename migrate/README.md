# readme

**Data Source**: 

[百度迁徙](http://qianxi.baidu.com/)

**Reference Links**:

若使用Python采集数据Scrape data with Python [参考代码](https://blog.csdn.net/qq_44315987/article/details/104118498)。文档中的migrant_flow.R 是用R采集数据的代码。

地图设计美学Aesthetics [mapping flow](http://spatial.ly/2015/03/mapping-flows/)

地图背景色Map Background Colour [midnight blue](https://colorpalettes.net/color-palette-3860/ )




#### 如果把人口和信息的数据映射到空间上，我们就可以从中感受到这些数据的流动。当看到下面这两张地图时，你是否会惊叹于数据之美？！

<img src="https://i1.wp.com/spatial.ly/wp-content/uploads/2014/09/home_work_print.jpg?w=450" alt="home2work" style="zoom:150%;" />

[From Home to Work](https://i1.wp.com/spatial.ly/wp-content/uploads/2014/09/home_work_print.jpg?w=450)

第一张地图画出了英国通勤人口的移动轨迹。每个人上下班时都会有自己的行动轨迹，从人口整体来看每一条线路都细若游丝，汇聚起来后却形成了一个个箭簇状的中心。其中，最明亮的和线条最密集的就是伦敦，它是人们工作中心的中心。



![Facebook Usage](https://engineering.fb.com/wp-content/uploads/2018/06/BLOG-IMAGE-CROP_CX-Map.jpg)

[Facebook Usage](https://engineering.fb.com/wp-content/uploads/2018/06/BLOG-IMAGE-CROP_CX-Map.jpg)

第二张图则非常形象地绘制了Facebook在全球形成的通讯网络，1 条线表示 1 对脸书用户间的邮件往来。每次发送跨洋邮件时，你与彼岸的笔友就会留下这样一条无形的纽带。世界的确是变小了呢！

通勤地图使用的是英国统计处（Office for National Statistics）公开的个人在工作和居住地点，Facebook地图利用的是个人与个人之间的通讯，以上的地图直接使用了个体层面的数据。



## 模仿

在这次的探索（模仿）中我使用的数据并不是个人层面的，百度迁徙网站只有城市层面的数据。城市与城市之间的人口迁徙的比重（示例见表1）也属于关系型数据，也可以用来表征联系的紧密程度，而各个城市的迁徙规模指数（示例见表2）则可以用来作为权重计算人口流入的相对规模。且不论巨型互联网公司如何收集个人的迁徙数据，自然是要有无数移动设备用户的地理定位，也不论它是如何计算迁徙规模指数，在后面的探索中发现该指数还是比较符合常识判断的。



| city_name来源城市 | province_name来源省份 | value占比（%） | city_to流入城市 |
| ----------------- | --------------------- | -------------- | --------------- |
| 廊坊市            | 河北省                | 18.94          | 北京            |
| 保定市            | 河北省                | 7.25           | 北京            |
| 天津市            | 天津市                | 4.57           | 北京            |
| 张家口市          | 河北省                | 3.61           | 北京            |
| 承德市            | 河北省                | 2.85           | 北京            |
| 邯郸市            | 河北省                | 2.73           | 北京            |
| 石家庄市          | 河北省                | 2.11           | 北京            |
| 唐山市            | 河北省                | 1.85           | 北京            |
| 沧州市            | 河北省                | 1.7            | 北京            |
| 邢台市            | 河北省                | 1.55           | 北京            |

表1：北京市的迁入来源地及占比，baiduqianxi.com提供了前100个城市的占比，这里展示前10位。



| 日期     | 迁入规模指数 | 城市 |
| -------- | ------------ | ---- |
| 20200101 | 6.318778     | 北京 |
| 20200102 | 6.466198     | 北京 |
| 20200103 | 8.910162     | 北京 |
| 20200104 | 10.51497     | 北京 |
| 20200105 | 13.85845     | 北京 |
| 20200106 | 11.50806     | 北京 |
| 20200107 | 10.6968      | 北京 |
| 20200108 | 11.18438     | 北京 |
| 20200109 | 11.48272     | 北京 |
| 20200110 | 10.83556     | 北京 |

表2：1月1日到1月10日间北京市的迁入规模指数



##### 利用上面两个指标、城市的空间位置，学习一下上面的两张地图的制图美学（有点黑客帝国呃），大约就可以得到下图（和想要的炫酷风格相差甚远呵呵呵）。庞杂的网络中间有一颗若隐若现的五角星⭐，如果Baidu没有对迁徙数据做过处理的话，这真是太奇妙了：   

</br>

![](https://github.com/xyangwu/whosesociety-data/blob/master/migrate/baidumigrate/fig/p2_1_1.png?raw=true)

   

##### 然后，再看迁徙规模比较大的城市有哪些：    

  

![](https://github.com/xyangwu/whosesociety-data/blob/master/migrate/baidumigrate/fig/p3_1.png?raw=true)
  
  
##### 再然后，聚焦到个别城市群落来看人口的流动规律：     
  
  
- 上海和苏州、无锡，杭州和绍兴、湖州、嘉兴间的人流规模相对较大。

![](https://github.com/xyangwu/whosesociety-data/blob/master/migrate/baidumigrate/fig/p_csj_label_1.png?raw=true)



- 成渝城市群中，成都和重庆为两极

![p_cy_label_1](https://github.com/xyangwu/whosesociety-data/blob/master/migrate/baidumigrate/fig/p_cy_label_1.png?raw=true)

………… 举例到此为止 …………



--------

##### 最后合成一张图来看看。

其中的曲线是在windows自带的画图工具中贴上去的，其实可以用抛物线或三角函数之类的曲线公式计算出再用R或其他的工具画上去，但是还要学习对我来说真的令人烦躁（其实是因为懒）；文字标签是用PDF阅读器之类直接了当地加上去，illustrator太贵了，在R里面找到放置标签的坐标也是太烧脑了，推介PDF-XChange ，界面太友好了呵呵呵：

<img src="https://github.com/xyangwu/whosesociety-data/blob/master/migrate/baidumigrate/fig/p_allcity_line_1.png?raw=true" alt="p_allcity_line_1" style="zoom:100%;" />