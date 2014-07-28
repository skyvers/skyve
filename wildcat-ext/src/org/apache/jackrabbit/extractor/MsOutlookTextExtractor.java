/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.jackrabbit.extractor;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;

import org.apache.poi.hsmf.MAPIMessage;

/**
 * Text extractor for Microsoft Outlook messages.
 */
public class MsOutlookTextExtractor extends AbstractTextExtractor {
    /**
     * Force loading of dependent class.
     */
    static {
        MAPIMessage.class.getName();
    }

    /**
     * Creates a new <code>MsOutlookTextExtractor</code> instance.
     */
    public MsOutlookTextExtractor() {
        super(new String[]{"application/vnd.ms-outlook"});
    }

    //-------------------------------------------------------< TextExtractor >

    /**
     * {@inheritDoc}
     * Returns an empty reader if an error occured extracting text from
     * the outlook message.
     */
    public Reader extractText(InputStream stream,
                              String type,
                              String encoding) throws IOException {
        try {
        	MAPIMessage message = new MAPIMessage(stream);
        	StringBuffer buffer = new StringBuffer();
        	buffer.append(message.getDisplayFrom()).append('\n');
        	buffer.append(message.getDisplayTo()).append('\n');
        	buffer.append(message.getSubject()).append('\n');
        	buffer.append(message.getTextBody());
            return new StringReader(buffer.toString());
        } catch (Exception e) {
System.err.println("Failed to extract Message content " + e.getMessage());
            return new StringReader("");
        } finally {
            stream.close();
        }
    }

}
