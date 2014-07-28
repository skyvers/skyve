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
import java.util.logging.Level;

import org.apache.poi.hwpf.extractor.WordExtractor;
import org.skyve.wildcat.util.UtilImpl;

/**
 * Text extractor for Microsoft Word documents.
 */
public class MsWordTextExtractor extends AbstractTextExtractor {
	/**
	 * Force loading of dependent class.
	 */
	static {
		WordExtractor.class.getName();
	}

	/**
	 * Creates a new <code>MsWordTextExtractor</code> instance.
	 */
	public MsWordTextExtractor() {
		super(new String[] { "application/vnd.ms-word", "application/msword" });
	}

	// -------------------------------------------------------< TextExtractor >

	/**
	 * {@inheritDoc} Returns an empty reader if an error occured extracting text
	 * from the word document.
	 */
	@Override
	public Reader extractText(InputStream stream, String type, String encoding)
	throws IOException {
		try {
			return new StringReader(new WordExtractor(stream).getText());
		}
		catch (Exception e) {
			UtilImpl.LOGGER.log(Level.WARNING,
					"Failed to extract Word text content", e);
			return new StringReader("");
		}
		finally {
			stream.close();
		}
	}
}
