package com.suncd.epm.cm.dao;

import com.suncd.epm.cm.domain.EcOrderPayment;
import org.apache.ibatis.annotations.Param;
import java.util.List;

/**
 * 订单支付单(EcOrderPayment)表数据库访问层
 *
 * @author makejava
 * @since 2020-05-27 09:40:38
 */
public interface EcOrderPaymentDao {

    /**
     * 通过ID查询单条数据
     *
     * @param id 主键
     * @return 实例对象
     */
    EcOrderPayment queryById(Object id);

    /**
     * 查询指定行数据
     *
     * @param orderIds 查询起始位置
     * @return 对象列表
     */
    List<EcOrderPayment> queryAllByLimit(@Param("orderIds") List<Long> orderIds);


    /**
     * 通过实体作为筛选条件查询
     *
     * @param ecOrderPayment 实例对象
     * @return 对象列表
     */
    List<EcOrderPayment> queryAll(EcOrderPayment ecOrderPayment);

    /**
     * 新增数据
     *
     * @param ecOrderPayment 实例对象
     * @return 影响行数
     */
    int insert(EcOrderPayment ecOrderPayment);

    /**
     * 修改数据
     *
     * @param ecOrderPayment 实例对象
     * @return 影响行数
     */
    int update(EcOrderPayment ecOrderPayment);

    /**
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 影响行数
     */
    int deleteById(Object id);

}