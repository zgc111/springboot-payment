package com.suncd.epm.cm.service;

import com.alipay.api.AlipayApiException;
import com.alipay.api.AlipayClient;
import com.alipay.api.AlipayResponse;
import com.alipay.api.DefaultAlipayClient;
import com.alipay.api.request.AlipayTradeCancelRequest;
import com.alipay.api.request.AlipayTradePrecreateRequest;
import com.alipay.api.request.AlipayTradeQueryRequest;
import com.alipay.api.response.AlipayTradeCancelResponse;
import com.alipay.api.response.AlipayTradePrecreateResponse;
import com.alipay.api.response.AlipayTradeQueryResponse;
import com.google.gson.Gson;
import com.suncd.epm.cm.domain.EcOrderPayQrCode;
import com.suncd.epm.cm.domain.EcOrderPayment;
import com.suncd.epm.cm.domain.PayBizContent;
import com.suncd.epm.cm.utils.ZxingUtils;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

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
    private String alipayPublicKey;
    @Value("${sign_type}")
    private String signType;
    @Value("${notify_url}")
    private String notifyUrl;
    @Autowired
    private EcOrderPaymentService ecOrderPaymentService;
    @Autowired
    private PayBizContentService payBizContentService;

    public AlipayClient getAlipayClient() {
        return new DefaultAlipayClient
            (serverUrl, appId, privateKey, "json",
                "utf-8", alipayPublicKey, "RSA2");
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public AlipayTradePrecreateResponse createOrderPayQrCode(EcOrderPayQrCode ecOrderPayQrCode) {
        //查询订单
        List<EcOrderPayment> ecOrderPayments = ecOrderPaymentService.queryAllByLimit(ecOrderPayQrCode.getOrderIds());
        if (ecOrderPayQrCode.getOrderIds().size() != ecOrderPayments.size()) {
            log.error("订单支付信息异常");
            return null;
        }
        //计算订单总支付金额
        Double money = ecOrderPayments.stream().map(EcOrderPayment::getTotalActualPrice).reduce(Double::sum).orElse(0.00);
        //生成唯一支付id
        String outTradeNo = String.valueOf(System.currentTimeMillis());
        //组装支付请求
        AlipayTradePrecreateResponse response = createOrderPayQrCode(String.valueOf(money), outTradeNo, ecOrderPayQrCode);
        //修改对应订单支付状态
        return response;
    }

    public AlipayTradePrecreateResponse createOrderPayQrCode(String money, String outTradeNo, EcOrderPayQrCode ecOrderPayQrCode) {
        AlipayClient alipayClient = getAlipayClient();
        //创建API对应的request类
        PayBizContent payBizContent = new PayBizContent();

        payBizContent.setOutTradeNo(outTradeNo);
        payBizContent.setStoreId("NJ_001");
        payBizContent.setSubject("随机金额" + money);
        payBizContent.setTimeoutExpress("2m");
        payBizContent.setTotalAmount(money);
        String orderIds = ecOrderPayQrCode.getOrderIds().stream().map(order -> order + ",").collect(Collectors.joining());
        payBizContent.setBody(orderIds);
        payBizContent.setStoreId(outTradeNo);
//        payBizContent.setAliPayStoreId(outTradeNo);
        payBizContent.setOperatorId(String.valueOf(ecOrderPayQrCode.getOperatorId()));
        payBizContent.setTerminalId(outTradeNo);
        String toString = new Gson().toJson(payBizContent);
        AlipayTradePrecreateRequest request = new AlipayTradePrecreateRequest();
        request.setNotifyUrl(notifyUrl);
        //订单允许的最晚付款时间
        request.setBizContent(toString);
        System.out.println(request.getBizContent());
        try {
            AlipayTradePrecreateResponse response = alipayClient.execute(request);
            System.out.println(response.getCode());
            if ("10000".equals(response.getCode())) {
                dumpResponse(response);
                // 需要修改为运行机器上的路径
                String filePath = String.format("C:/Users/change/Desktop/qr-%s.png",
                    response.getOutTradeNo());
                log.info("filePath:" + filePath);
                ZxingUtils.getQRCodeImge(response.getQrCode(), 256, filePath);
            } else {
                log.error("不支持的交易状态，交易返回异常!!!");
                throw new RuntimeException("不支持的交易状态，交易返回异常!!!");
            }
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
        List<Long> orderIds = Arrays.stream(payBizContent.getSubject().split(","))
            .map(Long::valueOf)
            .collect(Collectors.toList());
        List<EcOrderPayment> ecOrderPayments = ecOrderPaymentService.queryAllByLimit(orderIds);
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
            AlipayTradeCancelResponse response = getAlipayClient().execute(request);
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
    public AlipayTradeQueryResponse getTradesByOutTradeNo(String outTradeNo) {
        AlipayTradeQueryRequest request = new AlipayTradeQueryRequest();
        request.setBizContent("{" +
            "    \"out_trade_no\":\"" + outTradeNo + "\" }");
        try {
            System.out.println(request.getBizContent());
            AlipayTradeQueryResponse response = getAlipayClient().execute(request);
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
}
