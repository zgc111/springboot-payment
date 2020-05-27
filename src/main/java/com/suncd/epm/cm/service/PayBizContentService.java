package com.suncd.epm.cm.service;

import com.suncd.epm.cm.domain.PayBizContent;

import java.util.List;

/**
 * (PayBizContent)表服务接口
 *
 * @author makejava
 * @since 2020-05-27 11:19:36
 */
public interface PayBizContentService {

    /**
     * 通过ID查询单条数据
     *
     * @param outTradeNo 主键
     * @return 实例对象
     */
    PayBizContent queryById(String outTradeNo);

    /**
     * 查询多条数据
     *
     * @param offset 查询起始位置
     * @param limit  查询条数
     * @return 对象列表
     */
    List<PayBizContent> queryAllByLimit(int offset, int limit);

    /**
     * 新增数据
     *
     * @param payBizContent 实例对象
     * @return 实例对象
     */
    PayBizContent insert(PayBizContent payBizContent);

    /**
     * 修改数据
     *
     * @param payBizContent 实例对象
     * @return 实例对象
     */
    PayBizContent update(PayBizContent payBizContent);

    /**
     * 通过主键删除数据
     *
     * @param outTradeNo 主键
     * @return 是否成功
     */
    boolean deleteById(String outTradeNo);

}