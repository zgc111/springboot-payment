package com.suncd.epm.cm.service;

import com.alipay.api.AlipayApiException;
import com.alipay.api.AlipayClient;
import com.alipay.api.AlipayResponse;
import com.alipay.api.DefaultAlipayClient;
import com.alipay.api.internal.util.AlipaySignature;
import com.alipay.api.request.*;
import com.alipay.api.response.*;
import com.google.gson.Gson;
import com.suncd.epm.cm.domain.EcOrderPayQrCode;
import com.suncd.epm.cm.domain.EcOrderPayment;
import com.suncd.epm.cm.domain.PayBizContent;
import com.suncd.epm.cm.domain.TradeBillSellQuery;
import com.suncd.epm.cm.utils.ZxingUtils;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.servlet.http.HttpServletRequest;
import java.util.*;
import java.util.stream.Collectors;

import static java.lang.Math.random;

/**
 * @author YangQ
 * @date 2020/5/27 13:54
 */
@Service
@Log4j2
public class AliPayOrderServiceImpl implements AliPayOrderService {
    @Value("${open_api_domain}")
    private String serverUrl;
    @Value("${appid}")
    private String appId;
    @Value("${private_key}")
    private String privateKey;
    @Value("${alipay_public_key}")
    private String aliPayPublicKey;
    @Value("${sign_type}")
    private String signType;
    @Value("${notify_url}")
    private String notifyUrl;
    @Value("${return_url}")
    private String returnUrl;
    @Value("${format}")
    private String format;
    @Value("${charset}")
    private String charset;
    @Value("${timeout_express}")
    private String timeoutExpress;
    @Autowired
    private EcOrderPaymentService ecOrderPaymentService;
    @Autowired
    private PayBizContentService payBizContentService;
    private Gson gson = new Gson();

    public AlipayClient getAliPayClient() {
        return new DefaultAlipayClient
            (serverUrl, appId, privateKey, format,
                charset, aliPayPublicKey, signType);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public AlipayTradePrecreateResponse createOrderPayQrCode(EcOrderPayQrCode ecOrderPayQrCode) {
        //查询订单
        List<EcOrderPayment> ecOrderPayments = ecOrderPaymentService
            .queryAllByLimit(Collections.singletonList(ecOrderPayQrCode.getOrderId()))
            .stream().filter(payment -> 0 == payment.getPaymentStatus()).collect(Collectors.toList());
        if (ecOrderPayments.size() != 1) {
            log.error("订单支付信息异常");
            throw new RuntimeException("订单支付信息异常");
        }
        EcOrderPayment ecOrderPayment = ecOrderPayments.get(0);
        //计算订单总支付金额
        Double money = ecOrderPayment.getTotalActualPrice();
        //生成唯一支付id
        String outTradeNo = String.valueOf(System.currentTimeMillis());
        //组装支付请求
        AlipayTradePrecreateResponse response = createOrderPayQrCode(String.valueOf(money), outTradeNo, ecOrderPayQrCode);
        //修改对应订单支付状态
        ecOrderPayment.setPaymentStatus(4);
        ecOrderPaymentService.update(ecOrderPayment);
        return response;
    }

    @Override
    public AlipayTradePrecreateResponse createPayQrCode(Long orderId) {
        int rand = (int) (random() * 10);
//        int money = 5;
        int money = rand == 0 ? 1 : rand;
        AlipayTradePrecreateRequest request = new AlipayTradePrecreateRequest();
        request.setNotifyUrl(notifyUrl);
        PayBizContent payBizContent = new PayBizContent();
        payBizContent.setOutTradeNo(String.valueOf(orderId));
        payBizContent.setStoreId("NJ_001");
        payBizContent.setSubject("订单金额" + money);
        payBizContent.setTimeoutExpress(timeoutExpress);
        payBizContent.setTotalAmount(String.valueOf(money));
        String orderIds = String.valueOf(orderId);
        payBizContent.setBody(orderIds);
        String toString = gson.toJson(payBizContent);
        //订单允许的最晚付款时间
        request.setBizContent(toString);
        System.out.println(request.getBizContent());
        System.out.println(request.getNotifyUrl());
        try {
            AlipayTradePrecreateResponse response = getAliPayClient().execute(request);
            System.out.println(response.getCode());
            if ("10000".equals(response.getCode())) {
                dumpResponse(response);
                // 需要修改为运行机器上的路径
                String filePath = String.format("C:/Users/change/Desktop/qr-%s.png",
                    System.currentTimeMillis());
                log.info("filePath:" + filePath);
                ZxingUtils.getQrCodeImage(response.getQrCode(), 256, filePath);
            } else {
                log.error("不支持的交易状态，交易返回异常!!!");
                throw new RuntimeException("不支持的交易状态，交易返回异常!!!");
            }
        } catch (AlipayApiException e) {
            throw new RuntimeException(e.getMessage());
        }
        return null;
    }

    public AlipayTradePrecreateResponse createOrderPayQrCode(String money, String outTradeNo, EcOrderPayQrCode ecOrderPayQrCode) {
        AlipayClient alipayClient = getAliPayClient();
        //创建API对应的request类
        PayBizContent payBizContent = new PayBizContent();

        payBizContent.setOutTradeNo(outTradeNo);
        payBizContent.setStoreId("NJ_001");
        payBizContent.setSubject("订单金额" + money);
        payBizContent.setTimeoutExpress(timeoutExpress);
        payBizContent.setTotalAmount(money);
        String orderIds = String.valueOf(ecOrderPayQrCode.getOrderId());
        payBizContent.setBody(orderIds);
        payBizContent.setStoreId(outTradeNo);
//        payBizContent.setAliPayStoreId(outTradeNo);
        payBizContent.setOperatorId(String.valueOf(ecOrderPayQrCode.getOperatorId()));
        payBizContent.setTerminalId(outTradeNo);
        String toString = gson.toJson(payBizContent);
        AlipayTradePrecreateRequest request = new AlipayTradePrecreateRequest();
        request.setNotifyUrl(notifyUrl);
        //订单允许的最晚付款时间
        request.setBizContent(toString);
        System.out.println(request.getBizContent());
        System.out.println(request.getNotifyUrl());
        try {
            AlipayTradePrecreateResponse response = alipayClient.execute(request);
            System.out.println(response.getCode());
            if ("10000".equals(response.getCode())) {
                dumpResponse(response);
                // 需要修改为运行机器上的路径
                String filePath = String.format("C:/Users/change/Desktop/qr-%s.png",
                    response.getOutTradeNo());
                log.info("filePath:" + filePath);
                ZxingUtils.getQrCodeImage(response.getQrCode(), 256, filePath);
            } else {
                log.error("不支持的交易状态，交易返回异常!!!");
                throw new RuntimeException("不支持的交易状态，交易返回异常!!!");
            }
            payBizContent.setTradeStatus("WAIT_BUYER_PAY");
            payBizContentService.insert(payBizContent);
            return response;
        } catch (AlipayApiException e) {
            throw new RuntimeException(e.getMessage());
        }
    }

    private void dumpResponse(AlipayResponse response) {
        if (response != null) {
            log.info(String.format("code:%s, msg:%s", response.getCode(), response.getMsg()));
            if (StringUtils.isNotEmpty(response.getSubCode())) {
                log.info(String.format("subCode:%s, subMsg:%s", response.getSubCode(),
                    response.getSubMsg()));
            }
            log.info("body:" + response.getBody());
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public String tradeCancelQrCodeByOutTradeNo(String outTradeNo) {
        //查询支付信息是否存在
        PayBizContent payBizContent = payBizContentService.queryById(outTradeNo);
        if (Objects.isNull(payBizContent)) {
            return "支付信息不存在";
        }
        //查询支付宝侧是否存在这笔订单
        AlipayTradeQueryResponse queryResponse = getTradesByOutTradeNo(outTradeNo);
        //取消支付宝侧订单
        if ("10000".equals(queryResponse.getCode())) {
            tradeCancelByOutTradeNo(outTradeNo);
        }
        //删除本地支付信息
        payBizContentService.deleteById(outTradeNo);
        //修改对应订单支付信息
        List<EcOrderPayment> ecOrderPayments = ecOrderPaymentService.queryAllByLimit(Collections.singletonList(Long.valueOf(payBizContent.getBody())));
        for (EcOrderPayment payment : ecOrderPayments) {
            payment.setPaymentStatus(1);
            ecOrderPaymentService.update(payment);
        }
        return "success";
    }

    @Override
    public String tradeCancelByOutTradeNo(String outTradeNo) {
        AlipayTradeCancelRequest request = new AlipayTradeCancelRequest();
        request.setBizContent("{" +
            "    \"out_trade_no\":\"" + outTradeNo + "\" }");
        try {
            AlipayTradeCancelResponse response = getAliPayClient().execute(request);
            if ("10000".equals(response.getCode())) {
                return Boolean.TRUE.toString();
            } else {
                log.error("不支持的交易状态，交易返回异常!!!");
                return Boolean.FALSE.toString();
            }
        } catch (AlipayApiException e) {
            log.error("不支持的交易状态，交易返回异常!!!,原因:{}", e.getMessage());
            return Boolean.FALSE.toString();
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public String tradeRefundByOutTradeNo(String outTradeNo) {
        //查询支付宝侧订单记录是否存在
        AlipayTradeQueryResponse queryResponse = getTradesByOutTradeNo(outTradeNo);
        //判断交易状态
        if (queryResponse == null) {
            return "没有此订单";
        }
        if (!"10000".equals(queryResponse.getCode())) {
            return queryResponse.getSubMsg();
        }
        if (!queryResponse.getTradeStatus().equals("TRADE_SUCCESS")) {
            return "此订单不可退款";
        }
        //查询本地交易支付记录
        PayBizContent payBizContent = payBizContentService.queryById(outTradeNo);
        if (Objects.isNull(payBizContent)) {
            return "此支付单对应系统支付订单异常";
        }
        Long orderId = Long.valueOf(payBizContent.getBody());
        List<EcOrderPayment> ecOrderPayments = ecOrderPaymentService.queryAllByLimit(Collections.singletonList(orderId));
        if (ecOrderPayments.isEmpty()) {
            return "此支付单对应系统订单异常";
        }
        EcOrderPayment ecOrderPayment = ecOrderPayments.get(0);
        //本地退款
        ecOrderPayment.setPaymentStatus(1);
        ecOrderPaymentService.update(ecOrderPayment);
        payBizContent.setTradeStatus("TRADE_CLOSED");
        payBizContentService.update(payBizContent);
        //支付宝侧退款
        AlipayTradeRefundRequest request = new AlipayTradeRefundRequest();
        PayBizContent returnPayBizContent = new PayBizContent();
        returnPayBizContent.setOutTradeNo(outTradeNo);
        returnPayBizContent.setRefundAmount(queryResponse.getTotalAmount());
        request.setBizContent(gson.toJson(returnPayBizContent));
        try {
            AlipayTradeRefundResponse refundResponse = getAliPayClient().execute(request);
            if ("10000".equals(refundResponse.getCode())) {
                return "退款成功";
            } else {
                log.error("不支持的交易状态，交易返回异常!!!");
                throw new RuntimeException(refundResponse.getSubMsg());
            }
        } catch (AlipayApiException e) {
            throw new RuntimeException(e.getMessage());
        }
    }

    @Override
    public AlipayDataBillSellQueryResponse tradesBillSellQuery(TradeBillSellQuery tradesBillSellQuery) {
        AlipayDataBillSellQueryRequest request = new AlipayDataBillSellQueryRequest();
        String bizContent = gson.toJson(tradesBillSellQuery);
        log.debug("参数:{}", bizContent);
        request.setBizContent(bizContent);
        System.out.println(gson.toJson(request));
        try {
            AlipayDataBillAccountlogQueryRequest request1 = new AlipayDataBillAccountlogQueryRequest();
            request.setBizContent(bizContent);
            AlipayDataBillAccountlogQueryResponse response1 = getAliPayClient().execute(request1);
            AlipayDataBillSellQueryResponse response = getAliPayClient().execute(request);
            if (response.isSuccess()) {
                System.out.println("调用成功");
                return response;
            } else {
                System.out.println("调用失败");
                return response;
            }
        } catch (AlipayApiException e) {
            throw new RuntimeException(e.getMessage());
        }
    }

    @Override
    public AlipayTradeQueryResponse getTradesByOutTradeNo(String outTradeNo) {
        AlipayTradeQueryRequest request = new AlipayTradeQueryRequest();
        request.setBizContent("{" +
            "    \"out_trade_no\":\"" + outTradeNo + "\" }");
        try {
            System.out.println(request.getBizContent());
            AlipayTradeQueryResponse response = getAliPayClient().execute(request);
            if ("10000".equals(response.getCode())) {
                return response;
            } else {
                log.error("不支持的交易状态，交易返回异常!!!");
                return response;
            }
        } catch (AlipayApiException e) {
            log.error("不支持的交易状态，交易返回异常!!!,原因:{}", e.getMessage());
            return null;
        }
    }

    @Override
    public String tradesBillDownload(String billDate) {
        AlipayDataDataserviceBillDownloadurlQueryRequest request = new AlipayDataDataserviceBillDownloadurlQueryRequest();
        request.setBizContent("{" +
            "\"bill_type\":\"trade\"," +
            "\"bill_date\":\"" + billDate + "\"" +
            "  }");
        try {
            AlipayDataDataserviceBillDownloadurlQueryResponse response = getAliPayClient().execute(request);
            if (response.isSuccess()) {
                System.out.println("调用成功");
                System.out.println(response.getBillDownloadUrl());
                return response.getBillDownloadUrl();
            } else {
                System.out.println("调用失败");
                return response.getSubMsg();
            }
        } catch (AlipayApiException e) {
            return e.getErrMsg();
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void paymentAliCallBack(HttpServletRequest request) {
        Map<String, String[]> parmMap = request.getParameterMap();
        Map<String, String> paMap = getParm(parmMap);
        boolean verify = verifyAliAsyncCallBackParams(paMap);
        if (!verify) {
            log.error("支付宝支付回调发现异常回调,请立即排查!!!");
            return;
        }
        String tradeStatus = paMap.get("trade_status");
        if (tradeStatus.equals("WAIT_BUYER_PAY")) {
            log.debug("交易待支付业务");
        } else if (tradeStatus.equals("TRADE_CLOSED")) {
            log.debug("交易关闭业务");
        } else if (tradeStatus.equals("TRADE_SUCCESS")) {
            log.debug("交易成功业务");
            String outTradeNo = paMap.get("out_trade_no");
            //查询本地交易支付记录
            PayBizContent payBizContent = payBizContentService.queryById(outTradeNo);
            if (Objects.isNull(payBizContent)) {
                log.error("此支付单对应系统支付订单异常");
                return;
            }
            Long orderId = Long.valueOf(payBizContent.getBody());
            List<EcOrderPayment> ecOrderPayments = ecOrderPaymentService.queryAllByLimit(Collections.singletonList(orderId));
            if (ecOrderPayments.isEmpty()) {
                log.error("此支付单对应系统订单异常");
                return;
            }
            EcOrderPayment ecOrderPayment = ecOrderPayments.get(0);
            ecOrderPayment.setPaymentStatus(2);
            ecOrderPaymentService.update(ecOrderPayment);
            payBizContent.setTradeStatus(tradeStatus);
            payBizContentService.update(payBizContent);

        } else if (tradeStatus.equals("TRADE_FINISHED")) {
            log.debug("交易完成业务");
        } else {
            log.debug("不知名状态");
        }
    }

    private Boolean verifyAliAsyncCallBackParams(Map<String, String> map) {
        String sign = map.get("sign");
        map.remove("sign_type");
        String signContent = AlipaySignature.getSignCheckContentV2(map);
        try {
            return AlipaySignature.rsa256CheckContent(signContent, sign, aliPayPublicKey, "utf-8");
        } catch (AlipayApiException e) {
            log.debug(e.getMessage());
            return false;
        }
    }

    private Map<String, String> getParm(Map<String, String[]> parmMap) {
        Map<String, String> map = new HashMap<>(24);
        parmMap.keySet().forEach(key -> {
            String[] value = parmMap.get(key);
            map.put(key, value[0]);
        });
        return map;
    }

    @Override
    public String aliWapPay(String outTradeNo) {
        AlipayClient alipayClient = getAliPayClient();
        AlipayTradeWapPayRequest aliPayRequest = new AlipayTradeWapPayRequest();
        //同步通知
        aliPayRequest.setReturnUrl(returnUrl);
        // 异步通知
        aliPayRequest.setNotifyUrl(notifyUrl);
        PayBizContent payBizContent = new PayBizContent();
        payBizContent.setOutTradeNo(outTradeNo);
        int rand = (int) (random() * 10);
        int money = rand == 0 ? 1 : rand;
        payBizContent.setTotalAmount(String.valueOf(money));
        payBizContent.setSubject("手机支付");
        aliPayRequest.setBizContent(gson.toJson(payBizContent));
        // 调用SDK生成表单
        String form = null;
        try {
            log.debug("手机支付请求参数:{}", gson.toJson(aliPayRequest));
            AlipayTradeWapPayResponse response = alipayClient.pageExecute(aliPayRequest);
            log.debug("创建支付订单结果:{}", gson.toJson(response));
            form = response.getBody();
            log.debug("外部商户创建订单并支付:{}", form);
        } catch (AlipayApiException e) {
            log.error("发生了异常:{}", e.getMessage());
            e.printStackTrace();
        }
        return form;
    }
}
