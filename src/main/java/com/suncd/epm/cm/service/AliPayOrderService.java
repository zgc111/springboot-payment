package com.suncd.epm.cm.service;

import com.alipay.api.response.AlipayTradePrecreateResponse;
import com.alipay.api.response.AlipayTradeQueryResponse;
import com.suncd.epm.cm.domain.EcOrderPayQrCode;
import com.suncd.epm.cm.domain.PayBizContent;

/**
 * @author YangQ
 * @date 2020/5/27 13:50
 */
public interface AliPayOrderService {
    /**
     * 创建预支付单
     * @param ecOrderPayQrCode 请求参数
     * @return 结果
     */
    AlipayTradePrecreateResponse createOrderPayQrCode(EcOrderPayQrCode ecOrderPayQrCode);

    /**
     * 取消支付单
     * @param outTradeNo
     * @return
     */
    String tradeCancelQrCodeByOutTradeNo(String outTradeNo);

    /**
     * 取消交易
     * @param outTradeNo
     * @return
     */
    String tradeCancelByOutTradeNo(String outTradeNo);

    /**
     * 通过交易订单号查询支付侧订单
     * @param outTradeNo
     * @return
     */
    AlipayTradeQueryResponse getTradesByOutTradeNo(String outTradeNo);
}
