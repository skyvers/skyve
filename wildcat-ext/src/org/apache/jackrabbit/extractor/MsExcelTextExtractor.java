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

import org.apache.poi.hssf.extractor.ExcelExtractor;
import org.apache.poi.poifs.filesystem.POIFSFileSystem;

/**
 * Text extractor for Microsoft Excel sheets.
 */
public class MsExcelTextExtractor extends AbstractTextExtractor {
    /**
     * Force loading of dependent class.
     */
    static {
        POIFSFileSystem.class.getName();
    }

    /**
     * Creates a new <code>MsExcelTextExtractor</code> instance.
     */
    public MsExcelTextExtractor() {
        super(new String[] {
                "application/vnd.ms-excel",
                "application/msexcel",
                "application/excel"
        });
    }

    //-------------------------------------------------------< TextExtractor >

    /**
     * {@inheritDoc}
     */
    public Reader extractText(InputStream stream,
                              String type,
                              String encoding) throws IOException {
        try {
            POIFSFileSystem fs = new POIFSFileSystem(stream);
            return new StringReader(new ExcelExtractor(fs).getText());
        } catch (RuntimeException e) {
System.err.println("Failed to extract Excel text content. " + e.getMessage());
            return new StringReader("");
        } finally {
            stream.close();
        }
    }
}
