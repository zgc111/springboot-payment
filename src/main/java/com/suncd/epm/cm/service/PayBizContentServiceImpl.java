package com.suncd.epm.cm.service;

import com.suncd.epm.cm.dao.PayBizContentDao;
import com.suncd.epm.cm.domain.PayBizContent;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;

/**
 * (PayBizContent)表服务实现类
 *
 * @author makejava
 * @since 2020-05-27 11:19:37
 */
@Service("payBizContentService")
public class PayBizContentServiceImpl implements PayBizContentService {
    @Resource
    private PayBizContentDao payBizContentDao;

    /**
     * 通过ID查询单条数据
     *
     * @param outTradeNo 主键
     * @return 实例对象
     */
    @Override
    public PayBizContent queryById(String outTradeNo) {
        return this.payBizContentDao.queryById(outTradeNo);
    }

    /**
     * 查询多条数据
     *
     * @param offset 查询起始位置
     * @param limit  查询条数
     * @return 对象列表
     */
    @Override
    public List<PayBizContent> queryAllByLimit(int offset, int limit) {
        return this.payBizContentDao.queryAllByLimit(offset, limit);
    }

    /**
     * 新增数据
     *
     * @param payBizContent 实例对象
     * @return 实例对象
     */
    @Override
    public PayBizContent insert(PayBizContent payBizContent) {
        this.payBizContentDao.insert(payBizContent);
        return payBizContent;
    }

    /**
     * 修改数据
     *
     * @param payBizContent 实例对象
     * @return 实例对象
     */
    @Override
    public PayBizContent update(PayBizContent payBizContent) {
        this.payBizContentDao.update(payBizContent);
        return this.queryById(payBizContent.getOutTradeNo());
    }

    /**
     * 通过主键删除数据
     *
     * @param outTradeNo 主键
     * @return 是否成功
     */
    @Override
    public boolean deleteById(String outTradeNo) {
        return this.payBizContentDao.deleteById(outTradeNo) > 0;
    }
}