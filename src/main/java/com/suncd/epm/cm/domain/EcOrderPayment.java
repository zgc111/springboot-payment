package com.suncd.epm.cm.domain;

import lombok.Data;

import java.io.Serializable;
import java.util.Date;

/**
 * 订单支付单(EcOrderPayment)实体类
 *
 * @author makejava
 * @since 2020-05-27 09:40:33
 */
@Data
public class EcOrderPayment implements Serializable {
    private static final long serialVersionUID = -18950187444006505L;
    /**
     * 支付单号
     */
    private Object id;
    /**
     * 订单id
     */
    private Object orderId;
    /**
     * 账面应付
     */
    private Double totalListPrice;
    /**
     * 实际应付
     */
    private Double totalActualPrice;
    /**
     * 未支付退货金额
     */
    private Double unpaidReturnPrice;
    /**
     * 账面应付物流费用
     */
    private Double logisticsListPrice;
    /**
     * 账面应付商品费用
     */
    private Double totalGoodListPrice;
    /**
     * 由运营活动抵扣费用
     */
    private Double deductionPrice;
    /**
     * 支付状态, 1：待支付 2：已支付待核销 3：已核销,4:支付中
     */
    private Object paymentStatus;
    /**
     * 支付方式： 现金 微信 支付宝
     */
    private Object paymentMethod;
    /**
     * 支付时间
     */
    private Date paidAt;
    /**
     * 由谁确认支付
     */
    private Long confirmBy;
    /**
     * 收款方式
     */
    private Object collectionMethod;
    /**
     * 财务核销日期
     */
    private Date verifyAt;
    /**
     * 核销人员Id
     */
    private Long verifyBy;
    /**
     * 当前收款人
     */
    private Long currentPayee;
    /**
     * 收款人转移到谁
     */
    private Long payeeTransferTo;
    /**
     * 收款人转移状态 1:待确认,2:拒绝,3:同意
     */
    private Object payeeTransferStatus;
    /**
     * 发起收款人转移时间
     */
    private Date payeeTransferStartAt;


}