package com.suncd.epm.cm.controller;

import com.alipay.api.AlipayApiException;
import com.alipay.api.internal.util.AlipaySignature;
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
import java.util.HashMap;
import java.util.Map;

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


    @PostMapping(value = "/create-qr-code")
    public AlipayTradePrecreateResponse createOrderPayQrCode(@Validated @RequestBody EcOrderPayQrCode ecOrderPayQrCode) {
        return aliPayOrderService.createOrderPayQrCode(ecOrderPayQrCode);
    }

    @PostMapping(value = "/trades/cancel-qr-code")
    public String tradeCancelQrCodeByOutTradeNo(@RequestParam("outTradeNo") String outTradeNo) {
        return aliPayOrderService.tradeCancelQrCodeByOutTradeNo(outTradeNo);
    }

    @PutMapping("/trades/cancel")
    public String tradeCancelByOutTradeNo(@RequestParam("outTradeNo") String outTradeNo) {
        return aliPayOrderService.tradeCancelByOutTradeNo(outTradeNo);
    }

    @GetMapping("/trades/trade-no")
    public AlipayTradeQueryResponse getTradesByOutTradeNo(@RequestParam("outTradeNo") String outTradeNo) {
        return aliPayOrderService.getTradesByOutTradeNo(outTradeNo);
    }

    @PostMapping("/ali")
    public Map<String, String> aliCallBack(HttpServletRequest request) {
        Map<String, String[]> parmMap = request.getParameterMap();
        Map<String, String> paMap = getParm(parmMap);
        boolean v = verifyAliAsyncCallBackParams(paMap);
        System.out.println(v);
        if (!v) {
            log.error("支付宝支付回调发现异常回调,请立即派查!!!");
        }
        String tradeStatus = paMap.get("trade_status");
        System.out.println(tradeStatus);
        //修改对应订单支付状态
        return paMap;
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
}
