package com.suncd.epm.cm.utils.response;

import java.util.List;

/**
 * @author YangQing
 * @version 1.0.0
 */

public class PageResponse<T> {
    /**
     * 分页信息
     */
    private Page page;
    /**
     * 数据行
     */
    private List<T> rows;

    public PageResponse() {
    }

    public PageResponse(int totalRows, List<T> rows) {
        page = new Page(totalRows);
        this.rows = rows;
    }

    public PageResponse(int totalRows, String attachInfo, List<T> rows) {
        page = new Page(totalRows, attachInfo);
        this.rows = rows;
    }

    public Page getPage() {
        return page;
    }

    public void setPage(Page pages) {
        page = pages;
    }

    public List<T> getRows() {
        return rows;
    }

    public void setRows(List<T> rows) {
        this.rows = rows;
    }

    public class Page {
        private String attachInfo;  // 附加信息（可选参数），客户端提交的参数，服务端原样返回
        private int totalRows;  // 总行数

        public Page() {
        }

        public String getAttachInfo() {
            return attachInfo;
        }

        public void setAttachInfo(String attachInfo) {
            this.attachInfo = attachInfo;
        }

        public Page(int totalRows) {
            this.totalRows = totalRows;
        }

        public Page(int totalRows, String attachInfo) {
            this.attachInfo = attachInfo;
            this.totalRows = totalRows;
        }

        public int getTotalRows() {
            return totalRows;
        }

        public void setTotalRows(int totalRows) {
            this.totalRows = totalRows;
        }
    }
}

