# R, R Studio, and Data Visualization




<p>In this article I discuss how my classes in R and Rstudio have helped me create pleasing and informative visuals. <!--more--> <img src="/images/USdefaults.jpeg" /></p>
<p>Through the STAT 385 and STAT 448 Courses at UIUC, I am now able to create complex visual to represent data in a way that coneys a message to all audiences even if they do not have a strong understanding of the underlying statistics at work. The visual shown above conveys how the loan default rates in the United States vary from state to state, to add to this, this visual was not particularly difficutl to make! I simply made use of the <code>tidyverse</code> and <code>usmap</code> packages. You can find the full <code>.R</code> script used to make the visualization above <a href="/RScripts/data-challenge-1.R">here</a></p>
<p> </p>
<p>It is also possible to make very simple viualizations with base R; without using any special packages or learning anythign particularly complicated. For example, look at the following two visualizations.</p>
<pre class="r"><code>hist(x=faithful$eruptions, main =&quot;Histogram of Old Faithful Geyser Eruption Time&quot;,
     xlab = &quot;Eruption Time (mins)&quot;,
     xlim = c(1,6.2),
     ylim = c(0,0.82),
     breaks = 15,
     border = &quot;dodgerblue&quot;,
     col = &quot;navyblue&quot;,
     probability = TRUE)
lines(density(faithful$eruptions,na.rm=TRUE),col=&quot;red&quot;,lwd=2)
box()</code></pre>
<p><img src="/posts/2020-03-07-r-r-studio-and-data-visualization_files/figure-html/unnamed-chunk-1-1.png" width="672" /></p>
<pre class="r"><code>par(mfrow=c(1,2))
hist(faithful$waiting, main=&quot;Histogram of Waiting Time&quot;,
     xlab = &quot;Waiting Time (mins)&quot;,
     col = &quot;navyblue&quot;,
     border = &quot;white&quot;, probability = T)
box()
plot(x = faithful$waiting,y = faithful$eruptions, 
     main = &quot;Eruption Time vs. Waiting Time&quot;, 
     ylab = &quot;Eruption Time (mins)&quot;, 
     xlab = &quot;Waiting Time (mins)&quot;,
     pch=19, col=&quot;navyblue&quot;)
box()</code></pre>
<p><img src="/posts/2020-03-07-r-r-studio-and-data-visualization_files/figure-html/unnamed-chunk-2-1.png" width="672" /></p>

