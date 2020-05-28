package com.suncd.epm.cm.controller;

import com.alipay.api.response.AlipayTradePrecreateResponse;
import com.alipay.api.response.AlipayTradeQueryResponse;
import com.suncd.epm.cm.domain.EcOrderPayQrCode;
import com.suncd.epm.cm.service.AliPayOrderService;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;

/**
 * @author YangQ
 * @date 2020/5/26 17:41
 */
@RestController
@Log4j2
public class AliPayOrderController {
    @Value("${alipay_public_key}")
    private String aliPayPublicKey;
    @Autowired
    private AliPayOrderService aliPayOrderService;


    /**
     * 交易预创建
     * 收银员通过收银台或商户后台调用支付宝接口，生成二维码后，展示给用户，由用户扫描二维码完成订单支付
     *
     * @param ecOrderPayQrCode
     * @return
     */
    @PostMapping(value = "/create-qr-code")
    public AlipayTradePrecreateResponse createOrderPayQrCode(@Validated @RequestBody EcOrderPayQrCode ecOrderPayQrCode) {
        return aliPayOrderService.createOrderPayQrCode(ecOrderPayQrCode);
    }

    /**
     * 取消二维码
     * 如果生成的二维码没有扫码,则删除本地支付单
     * 如果已经扫码,则取消支付侧订单
     *
     * @param outTradeNo
     * @return
     */
    @PostMapping(value = "/trades/cancel-qr-code")
    public String tradeCancelQrCodeByOutTradeNo(@RequestParam("outTradeNo") String outTradeNo) {
        return aliPayOrderService.tradeCancelQrCodeByOutTradeNo(outTradeNo);
    }

    /**
     * 收单交易撤销
     * 支付交易返回失败或支付系统超时，调用该接口撤销交易。
     * 如果此订单用户支付失败，支付宝系统会将此订单关闭；
     * 如果用户支付成功，支付宝系统会将此订单资金退还给用户。
     * 注意：只有发生支付系统超时或者支付结果未知时可调用撤销，其他正常支付的单如需实现相同功能请调用申请退款API。
     * 提交支付交易后调用【查询订单API】，没有明确的支付结果再调用【撤销订单API】。
     *
     * @param outTradeNo
     * @return
     */
    @PutMapping("/trades/cancel")
    public String tradeCancelByOutTradeNo(@RequestParam("outTradeNo") String outTradeNo) {
        return aliPayOrderService.tradeCancelByOutTradeNo(outTradeNo);
    }

    /**
     * 收单交易退款(一笔全退)
     * 当交易发生之后一段时间内，由于买家或者卖家的原因需要退款时，卖家可以通过退款接口将支付款退还给买家，
     * 支付宝将在收到退款请求并且验证成功之后，按照退款规则将支付款按原路退到买家帐号上。
     * 交易超过约定时间（签约时设置的可退款时间）的订单无法进行退款 支付宝退款支持单笔交易分多次退款，
     * 多次退款需要提交原支付订单的商户订单号和设置不同的退款单号。
     * 一笔退款失败后重新提交，要采用原来的退款单号。总退款金额不能超过用户实际支付金额
     *
     * @param outTradeNo
     * @return
     */
    @PutMapping("/trades/refund")
    public String tradeRefundByOutTradeNo(@RequestParam("outTradeNo") String outTradeNo) {
        return aliPayOrderService.tradeRefundByOutTradeNo(outTradeNo);
    }

    /**
     * 收单线下交易查询
     * 该接口提供所有支付宝支付订单的查询，商户可以通过该接口主动查询订单状态，完成下一步的业务逻辑。
     * 需要调用查询接口的情况： 当商户后台、网络、服务器等出现异常，商户系统最终未接收到支付通知；
     * 调用支付接口后，返回系统错误或未知交易状态情况；
     * 调用alipay.trade.pay，返回INPROCESS的状态；
     * 调用alipay.trade.cancel之前，需确认支付状态；
     *
     * @param outTradeNo
     * @return
     */
    @GetMapping("/trades/trade-no")
    public AlipayTradeQueryResponse getTradesByOutTradeNo(@RequestParam("outTradeNo") String outTradeNo) {
        return aliPayOrderService.getTradesByOutTradeNo(outTradeNo);
    }

    /**
     * 交易回调(现在只处理支付成功的)
     *
     * @param request
     */
    @PostMapping("/trades/payment/ali-call-back")
    public void paymentAliCallBack(HttpServletRequest request) {
        log.debug("支付宝侧回调...");
        aliPayOrderService.paymentAliCallBack(request);
        log.debug("支付宝侧回调...");
    }


}
