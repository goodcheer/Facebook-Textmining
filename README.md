# Facebook-Textmining
In South Korea, there are Facebook web pages called "Bamboo Forest" where university students share their ideas anonymously. By scraping this pages, we are about to analyse what are the major issues around South Korean university students. 

<h2> Step1: Crawling Data </h2>
We parsed Facebook Page contents using Rfacebook Package.<br>
Scraped data from the date page was published until 2017-10-16.<br>
This code include only my part of scraping data. Rest of data (other universities) were scraped by other project members.
<a href="RfacebookCrawler.R"> RfacebookCrawler.R </a>

<h2> Step2: Cleansing Data </h2>
<ol>
<li> <b> Seperating Posts by documents. </b> <br>
          LSA method needs the each row of Document Term Matrix to be single document. However, there are some facebook page posts that includes 
          several documents. Therefore, we need to split those posts in to individual documents. <br>
          <a href="Post_seperation.md">Post-seperation.md </a> </li>
