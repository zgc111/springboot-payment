package com.suncd.epm.cm.service;

import com.suncd.epm.cm.domain.EcOrderPayment;
import com.suncd.epm.cm.dao.EcOrderPaymentDao;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;

/**
 * 订单支付单(EcOrderPayment)表服务实现类
 *
 * @author makejava
 * @since 2020-05-27 09:40:39
 */
@Service("ecOrderPaymentService")
public class EcOrderPaymentServiceImpl implements EcOrderPaymentService {
    @Resource
    private EcOrderPaymentDao ecOrderPaymentDao;

    /**
     * 通过ID查询单条数据
     *
     * @param id 主键
     * @return 实例对象
     */
    @Override
    public EcOrderPayment queryById(Object id) {
        return this.ecOrderPaymentDao.queryById(id);
    }

    /**
     * 查询多条数据
     *
     * @param orderIds 订单id集合
     * @return 对象列表
     */
    @Override
    public List<EcOrderPayment> queryAllByLimit(List<Long> orderIds) {
        return this.ecOrderPaymentDao.queryAllByLimit(orderIds);
    }

    /**
     * 新增数据
     *
     * @param ecOrderPayment 实例对象
     * @return 实例对象
     */
    @Override
    public EcOrderPayment insert(EcOrderPayment ecOrderPayment) {
        this.ecOrderPaymentDao.insert(ecOrderPayment);
        return ecOrderPayment;
    }

    /**
     * 修改数据
     *
     * @param ecOrderPayment 实例对象
     * @return 实例对象
     */
    @Override
    public EcOrderPayment update(EcOrderPayment ecOrderPayment) {
        this.ecOrderPaymentDao.update(ecOrderPayment);
        return this.queryById(ecOrderPayment.getId());
    }

    /**
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    @Override
    public boolean deleteById(Object id) {
        return this.ecOrderPaymentDao.deleteById(id) > 0;
    }
}