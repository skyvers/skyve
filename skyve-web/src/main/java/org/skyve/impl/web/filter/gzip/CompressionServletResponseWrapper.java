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
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

import org.skyve.impl.util.UtilImpl;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpServletResponseWrapper;

/**
 * Implementation of <b>HttpServletResponseWrapper</b> that works with
 * the CompressionServletResponseStream implementation..
 *
 * @author Amy Roh
 * @author Dmitri Valdin
 * @version $Revision: 500668 $, $Date: 2007-01-28 00:07:51 +0100 (Sun, 28 Jan 2007) $
 */

public class CompressionServletResponseWrapper extends HttpServletResponseWrapper {

    // ----------------------------------------------------- Constructor

    /**
     * Calls the parent constructor which creates a ServletResponse adaptor
     * wrapping the given response object.
     */

    public CompressionServletResponseWrapper(HttpServletResponse response) {
        super(response);
        origResponse = response;
        if (debug > 1) {
        	UtilImpl.LOGGER.info("CompressionServletResponseWrapper constructor gets called");
        }
    }


    // ----------------------------------------------------- Instance Variables

    /**
     * Original response
     */

    protected HttpServletResponse origResponse = null;

    /**
     * Descriptive information about this Response implementation.
     */

    protected static final String info = "CompressionServletResponseWrapper";

    /**
     * The ServletOutputStream that has been returned by
     * <code>getOutputStream()</code>, if any.
     */

    protected ServletOutputStream stream = null;


    /**
     * The PrintWriter that has been returned by
     * <code>getWriter()</code>, if any.
     */

    protected PrintWriter writer = null;

    /**
     * The threshold number to compress
     */
    protected int threshold = 0;

    /**
     * Debug level
     */
    private int debug = 0;

    /**
     * Content type
     */
    protected String contentType = null;

    // --------------------------------------------------------- Public Methods


    /**
     * Set content type
     */
    @Override
	public void setContentType(String contentType) {
        if (debug > 1) {
        	UtilImpl.LOGGER.info("setContentType to "+contentType);
        }
        this.contentType = contentType;
        origResponse.setContentType(contentType);
    }


    /**
     * Set threshold number
     */
    public void setCompressionThreshold(int threshold) {
        if (debug > 1) {
        	UtilImpl.LOGGER.info("setCompressionThreshold to " + threshold);
        }
        this.threshold = threshold;
    }


    /**
     * Set debug level
     */
    public void setDebugLevel(int debug) {
        this.debug = debug;
    }


    /**
     * Create and return a ServletOutputStream to write the content
     * associated with this Response.
     *
     * @exception IOException if an input/output error occurs
     */
    public ServletOutputStream createOutputStream() throws IOException {
        if (debug > 1) {
        	UtilImpl.LOGGER.info("createOutputStream gets called");
        }

        @SuppressWarnings("hiding")
		CompressionResponseStream stream = new CompressionResponseStream(origResponse);
        stream.setDebugLevel(debug);
        stream.setBuffer(threshold);

        return stream;

    }


    /**
     * Finish a response.
     */
    public void finishResponse() {
        try {
            if (writer != null) {
                writer.close();
            } else {
                if (stream != null)
                    stream.close();
            }
        } catch (@SuppressWarnings("unused") IOException e) {
        	// do nothing - not much we can do
        }
    }


    // ------------------------------------------------ ServletResponse Methods


    /**
     * Flush the buffer and commit this response.
     *
     * @exception IOException if an input/output error occurs
     */
    @Override
	public void flushBuffer() throws IOException {
        if (debug > 1) {
        	UtilImpl.LOGGER.info("flush buffer @ CompressionServletResponseWrapper");
        }
        ((CompressionResponseStream)stream).flush();

    }

    /**
     * Return the servlet output stream associated with this Response.
     *
     * @exception IllegalStateException if <code>getWriter</code> has
     *  already been called for this response
     * @exception IOException if an input/output error occurs
     */
    @Override
	public ServletOutputStream getOutputStream() throws IOException {

        if (writer != null)
            throw new IllegalStateException("getWriter() has already been called for this response");

        if (stream == null)
            stream = createOutputStream();
        if (debug > 1) {
        	UtilImpl.LOGGER.info("stream is set to "+stream+" in getOutputStream");
        }

        return (stream);

    }

    /**
     * Return the writer associated with this Response.
     *
     * @exception IllegalStateException if <code>getOutputStream</code> has
     *  already been called for this response
     * @exception IOException if an input/output error occurs
     */
    @Override
	public PrintWriter getWriter() throws IOException {

        if (writer != null)
            return (writer);

        if (stream != null)
            throw new IllegalStateException("getOutputStream() has already been called for this response");

        stream = createOutputStream();
        if (debug > 1) {
        	UtilImpl.LOGGER.info("stream is set to "+stream+" in getWriter");
        }
        //String charset = getCharsetFromContentType(contentType);
        String charEnc = origResponse.getCharacterEncoding();
        if (debug > 1) {
        	UtilImpl.LOGGER.info("character encoding is " + charEnc);
        }
        // HttpServletResponse.getCharacterEncoding() shouldn't return null
        // according the spec, so feel free to remove that "if"
        if (charEnc != null) {
            writer = new PrintWriter(new OutputStreamWriter(stream, charEnc));
        } else {
            writer = new PrintWriter(stream);
        }
        
        return (writer);

    }


    @Override
	public void setContentLength(int length) {
    	// no content length to set, as we dont know once compressed
    }

}
