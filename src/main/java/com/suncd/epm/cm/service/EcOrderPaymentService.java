package com.suncd.epm.cm.service;

import com.suncd.epm.cm.domain.EcOrderPayment;

import java.util.List;

/**
 * 订单支付单(EcOrderPayment)表服务接口
 *
 * @author makejava
 * @since 2020-05-27 09:40:39
 */
public interface EcOrderPaymentService {

    /**
     * 通过ID查询单条数据
     *
     * @param id 主键
     * @return 实例对象
     */
    EcOrderPayment queryById(Object id);

    /**
     * 查询多条数据
     *
     * @param orderIds 订单集合
     * @return 对象列表
     */
    List<EcOrderPayment> queryAllByLimit(List<Long> orderIds);

    /**
     * 新增数据
     *
     * @param ecOrderPayment 实例对象
     * @return 实例对象
     */
    EcOrderPayment insert(EcOrderPayment ecOrderPayment);

    /**
     * 修改数据
     *
     * @param ecOrderPayment 实例对象
     * @return 实例对象
     */
    EcOrderPayment update(EcOrderPayment ecOrderPayment);

    /**
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    boolean deleteById(Object id);

}