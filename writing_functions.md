writing_functions
================
Tianshu Liu
2022-10-27

function_name = function(argument){

function body

}

Z-score

``` r
x_vec = rnorm(25, mean = 7, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -1.500990402  0.320897130  0.401127168 -0.831927241  0.563619735
    ##  [6]  0.566276979 -0.269326562  0.009300717  0.766788286 -0.317809592
    ## [11] -1.008038018  0.373127000 -0.228162688 -1.022503912 -0.808395825
    ## [16] -1.212325020 -0.935430586 -1.666139873 -0.658323221  0.235431852
    ## [21]  1.574291147  1.075112881  1.115334510  1.823205078  1.634860457

``` r
# function
z_scores = function(x){
  if(!is.numeric(x)){
    stop("Z scores only work for numbers")
  }else if (length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  z = (x - mean(x)) / sd(x)
  z     # return value
}

z_scores(x_vec)
```

    ##  [1] -1.500990402  0.320897130  0.401127168 -0.831927241  0.563619735
    ##  [6]  0.566276979 -0.269326562  0.009300717  0.766788286 -0.317809592
    ## [11] -1.008038018  0.373127000 -0.228162688 -1.022503912 -0.808395825
    ## [16] -1.212325020 -0.935430586 -1.666139873 -0.658323221  0.235431852
    ## [21]  1.574291147  1.075112881  1.115334510  1.823205078  1.634860457

``` r
z_scores(x = 1:10)
```

    ##  [1] -1.4863011 -1.1560120 -0.8257228 -0.4954337 -0.1651446  0.1651446
    ##  [7]  0.4954337  0.8257228  1.1560120  1.4863011

``` r
z_scores(x = rbinom(100, 1, 0.6))
```

    ##   [1] -1.3559393  0.7301212  0.7301212  0.7301212  0.7301212  0.7301212
    ##   [7]  0.7301212  0.7301212  0.7301212 -1.3559393  0.7301212  0.7301212
    ##  [13] -1.3559393 -1.3559393  0.7301212  0.7301212  0.7301212 -1.3559393
    ##  [19]  0.7301212  0.7301212  0.7301212  0.7301212 -1.3559393  0.7301212
    ##  [25]  0.7301212 -1.3559393  0.7301212  0.7301212  0.7301212 -1.3559393
    ##  [31]  0.7301212 -1.3559393  0.7301212 -1.3559393  0.7301212  0.7301212
    ##  [37]  0.7301212  0.7301212 -1.3559393  0.7301212  0.7301212  0.7301212
    ##  [43]  0.7301212  0.7301212 -1.3559393  0.7301212  0.7301212 -1.3559393
    ##  [49]  0.7301212 -1.3559393  0.7301212  0.7301212  0.7301212  0.7301212
    ##  [55] -1.3559393 -1.3559393  0.7301212  0.7301212  0.7301212 -1.3559393
    ##  [61] -1.3559393  0.7301212 -1.3559393 -1.3559393  0.7301212 -1.3559393
    ##  [67]  0.7301212  0.7301212 -1.3559393 -1.3559393  0.7301212 -1.3559393
    ##  [73] -1.3559393  0.7301212 -1.3559393  0.7301212  0.7301212  0.7301212
    ##  [79]  0.7301212  0.7301212  0.7301212  0.7301212  0.7301212 -1.3559393
    ##  [85]  0.7301212  0.7301212 -1.3559393 -1.3559393  0.7301212  0.7301212
    ##  [91] -1.3559393  0.7301212  0.7301212 -1.3559393 -1.3559393 -1.3559393
    ##  [97]  0.7301212 -1.3559393  0.7301212 -1.3559393

Multiple outputs

``` r
mean_and_sd = function(x){
  
  if(!is.numeric(x)){
    stop("Z scores only work for numbers")
  }else if (length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
}

mean_and_sd(x = x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  7.14  3.98

``` r
mean_and_sd(x = 1:10)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1   5.5  3.03

``` r
mean_and_sd(x = rbinom(100, 1, .5))
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  0.43 0.498

``` r
x_vec = rnorm(n = 25000, mean = 17, sd = 4)
tibble(
  mean = mean(x_vec),
  sd = sd(x_vec)
)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  17.0  4.03

``` r
sim_mean_sd = function(n_obs, true_mean, true_sd){
  
  x = rnorm(n_obs, mean = true_mean, sd = true_sd)
  
  tibble(
    mean = mean(x),
    sd = sd(x)
  )
}

sim_mean_sd(n_obs = 25000, true_mean = 10, true_sd = 5)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.1  4.97

``` r
sim_mean_sd(25000, 10, 5)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.96  5.03

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

Use function to get reviews

``` r
read_page_reviews = function(url){
  dynamite_html = read_html(url)
  
  review_titles = 
    dynamite_html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()

  review_stars = 
    dynamite_html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()

  review_text = 
    dynamite_html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim() %>% 
    str_subset("The media could not be loaded.", negate = TRUE) %>% 
    str_subset("^$", negate = TRUE)

  reviews = tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
  
  reviews
}

url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

read_page_reviews(url = url)
```

    ## # A tibble: 10 × 3
    ##    title                                stars text                              
    ##    <chr>                                <dbl> <chr>                             
    ##  1 70’s and 80’s Schtick Comedy             5 …especially funny if you have eve…
    ##  2 Amazon Censorship                        5 I hope Amazon does not censor my …
    ##  3 Watch to say you did                     3 I know it's supposed to be a cult…
    ##  4 Best Movie Ever!                         5 We just love this movie and even …
    ##  5 Quirky                                   5 Good family film                  
    ##  6 Funny movie - can't play it !            1 Sony 4k player won't even recogni…
    ##  7 A brilliant story about teenage life     5 Napoleon Dynamite delivers dry hu…
    ##  8 HUHYAH                                   5 Spicy                             
    ##  9 Cult Classic                             4 Takes a time or two to fully appr…
    ## 10 Sweet                                    5 Timeless Movie. My Grandkids are …

``` r
base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
vec_url = str_c(base_url, c(1, 2, 3, 4, 5))

dynamic_reviews = 
  bind_rows(
    read_page_reviews(vec_url[1]),
    read_page_reviews(vec_url[2]),
    read_page_reviews(vec_url[3]),
    read_page_reviews(vec_url[4]),
    read_page_reviews(vec_url[5])
  )

dynamic_reviews %>% view()
```
