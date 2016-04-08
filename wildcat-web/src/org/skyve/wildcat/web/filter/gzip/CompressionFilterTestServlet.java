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

package org.skyve.wildcat.web.filter.gzip;

import java.io.IOException;
import java.util.Enumeration;

import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.content.MimeType;
import org.skyve.util.Util;

/**
 * Very Simple test servlet to test compression filter
 * @author Amy Roh
 * @version $Revision: 500668 $, $Date: 2007-01-28 00:07:51 +0100 (Sun, 28 Jan 2007) $
 */

public class CompressionFilterTestServlet extends HttpServlet {
    /**
	 * For Serialization
	 */
	private static final long serialVersionUID = 8114927744016828665L;

	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response)
        throws ServletException, IOException {

        try (ServletOutputStream out = response.getOutputStream()) {
	        response.setContentType(MimeType.plain.toString());
	        response.setCharacterEncoding(Util.UTF8);
	
	        Enumeration<?> e = request.getHeaders("Accept-Encoding");
	        while (e.hasMoreElements()) {
	            String name = (String)e.nextElement();
	            out.println(name);
	            if (name.indexOf("gzip") != -1) {
	                out.println("gzip supported -- able to compress");
	            }
	            else {
	                out.println("gzip not supported");
	            }
	        }
	
	
	        out.println("Compression Filter Test Servlet");
        }
    }
}

