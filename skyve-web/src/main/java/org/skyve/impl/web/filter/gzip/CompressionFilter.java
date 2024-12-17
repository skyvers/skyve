/*
* Licensed to the Apache Software Foundation (ASF) under one or more
* contributor license agreements.  See the NOTICE file distributed with
* this work for additional information regarding copyright ownership.
* The ASF licenses this file to You under the Apache License, Version 2.0
* (the "License"); you may not use this file except in compliance with
* the License.  You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

package org.skyve.impl.web.filter.gzip;

import java.io.IOException;
import java.util.Enumeration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Implementation of <code>jakarta.servlet.Filter</code> used to compress
 * the ServletResponse if it is bigger than a threshold.
 *
 * @author Amy Roh
 * @author Dmitri Valdin
 * @version $Revision: 467217 $, $Date: 2006-10-24 05:14:34 +0200 (Tue, 24 Oct 2006) $
 */

public class CompressionFilter implements Filter {

    private static final Logger LOGGER = LoggerFactory.getLogger(CompressionFilter.class);

    /**
     * The filter configuration object we are associated with.  If this value
     * is null, this filter instance is not currently configured.
     */
    private FilterConfig config = null;

    /**
     * Minimal reasonable threshold
     */
    private int minThreshold = 128;


    /**
     * The threshold number to compress
     */
    protected int compressionThreshold;

    /**
     * Debug level for this filter
     */
    private int debug = 0;

    /**
     * Place this filter into service.
     *
     * @param filterConfig The filter configuration object
     */

    @Override
	public void init(FilterConfig filterConfig) {

        config = filterConfig;
        if (filterConfig != null) {
            String value = filterConfig.getInitParameter("debug");
            if (value!=null) {
                debug = Integer.parseInt(value);
            } else {
                debug = 0;
            }
            String str = filterConfig.getInitParameter("compressionThreshold");
            if (str!=null) {
                compressionThreshold = Integer.parseInt(str);
                if (compressionThreshold != 0 && compressionThreshold < minThreshold) {
                    if (debug > 0) {
                    	LOGGER.info("compressionThreshold should be either 0 - no compression or >= " + minThreshold);
                    	LOGGER.info("compressionThreshold set to " + minThreshold);
                    }
                    compressionThreshold = minThreshold;
                }
            } else {
                compressionThreshold = 0;
            }

        } else {
            compressionThreshold = 0;
        }

    }

    /**
    * Take this filter out of service.
    */
    @Override
	public void destroy() {

        this.config = null;

    }

    /**
     * The <code>doFilter</code> method of the Filter is called by the container
     * each time a request/response pair is passed through the chain due
     * to a client request for a resource at the end of the chain.
     * The FilterChain passed into this method allows the Filter to pass on the
     * request and response to the next entity in the chain.<p>
     * This method first examines the request to check whether the client support
     * compression. <br>
     * It simply just pass the request and response if there is no support for
     * compression.<br>
     * If the compression support is available, it creates a
     * CompressionServletResponseWrapper object which compresses the content and
     * modifies the header if the content length is big enough.
     * It then invokes the next entity in the chain using the FilterChain object
     * (<code>chain.doFilter()</code>), <br>
     **/

    @Override
	public void doFilter(ServletRequest request,
							ServletResponse response,
							FilterChain chain)
	throws IOException, ServletException {
        if (debug > 0) {
        	LOGGER.info("@doFilter");
        }

        if (compressionThreshold == 0) {
            if (debug > 0) {
            	LOGGER.info("doFilter gets called, but compressionTreshold is set to 0 - no compression");
            }
            chain.doFilter(request, response);
            return;
        }

        boolean supportCompression = false;
        if (request instanceof HttpServletRequest) {
            if (debug > 1) {
            	LOGGER.info("requestURI = " + ((HttpServletRequest) request).getRequestURI());
            }

            // Are we allowed to compress ?
            String s = ((HttpServletRequest)request).getParameter("gzip");
            if ("false".equals(s)) {
                if (debug > 0) {
                	LOGGER.info("got parameter gzip=false --> don't compress, just chain filter");
                }
                chain.doFilter(request, response);
                return;
            }

            Enumeration<?> e =
                ((HttpServletRequest)request).getHeaders("Accept-Encoding");
            while (e.hasMoreElements()) {
                String name = (String)e.nextElement();
                if (name.indexOf("gzip") != -1) {
                    if (debug > 0) {
                    	LOGGER.info("supports compression");
                    }
                    supportCompression = true;
                } else {
                    if (debug > 0) {
                    	LOGGER.info("no support for compresion");
                    }
                }
            }
        }

        if (!supportCompression) {
            if (debug > 0) {
            	LOGGER.info("doFilter gets called wo compression");
            }
            chain.doFilter(request, response);
            return;
        } 

        if (response instanceof HttpServletResponse) {
            CompressionServletResponseWrapper wrappedResponse =
                new CompressionServletResponseWrapper((HttpServletResponse)response);
            wrappedResponse.setDebugLevel(debug);
            wrappedResponse.setCompressionThreshold(compressionThreshold);
            if (debug > 0) {
            	LOGGER.info("doFilter gets called with compression");
            }
            try {
                chain.doFilter(request, wrappedResponse);
            } finally {
                wrappedResponse.finishResponse();
            }
            return;
        }
    }

    /**
     * Set filter config
     * This function is equivalent to init. Required by Weblogic 6.1
     *
     * @param filterConfig The filter configuration object
     */
    public void setFilterConfig(FilterConfig filterConfig) {
        init(filterConfig);
    }

    /**
     * Return filter config
     * Required by Weblogic 6.1
     */
    public FilterConfig getFilterConfig() {
        return config;
    }

}

