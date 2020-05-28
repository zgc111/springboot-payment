# spring-boot-payment

1.需要开通应用
2.准备沙箱环境
https://openhome.alipay.com/platform/appDaily.htm?tab=info

来源如图: ![image](https://github.com/a736875071/spring-boot-payment/blob/master/src/main/resources/img/%E5%9F%BA%E7%A1%80%E7%8E%AF%E5%A2%83%E9%85%8D%E7%BD%AE%E6%9D%A5%E6%BA%90.png)

#支付宝网关
open_api_domain=https://openapi.alipaydev.com/gateway.do

#商户UID
pid=

#APPID
appid=

# 应用私钥(这个需要下载生成密钥工具地址:https://opendocs.alipay.com/open/291/105971#LDsXr)
private_key=

# 应用公钥
public_key=

#支付宝公钥
alipay_public_key=

# 签名类型: RSA->SHA1withRsa,RSA2->SHA256withRsa
sign_type=RSA2

#回调地址(需要映射到公网才能回调成功)
notify_url=http://xxxx/trades/payment/ali-call-back

format=json

charset=utf-8

#最后付款时间,创建后开始倒计时
timeout_express=5m

3.下载沙箱环境的支付宝app
![image](https://github.com/a736875071/spring-boot-payment/blob/master/src/main/resources/img/%E6%B2%99%E6%BC%8F%E7%8E%AF%E5%A2%83app.jpg)
4.使用沙箱账号登录测试
![image](https://github.com/a736875071/spring-boot-payment/blob/master/src/main/resources/img/%E6%B2%99%E6%BC%8F%E7%8E%AF%E5%A2%83%E8%B4%A6%E5%8F%B7.jpg)

注意:
    1.生成的付款二维码,如果没有扫描,通过
    https://opendocs.alipay.com/apis/api_1/alipay.trade.query
    是查询不到订单的
    

