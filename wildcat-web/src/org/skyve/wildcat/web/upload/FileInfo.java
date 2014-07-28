package org.skyve.wildcat.web.upload;

import java.io.File;

public class FileInfo 
{
	public String name = null, clientFileName = null, fileContentType = null;
	private byte[] fileContents = null;
	public File file = null;
	public StringBuffer sb = new StringBuffer(100);

	public void setFileContents(byte[] aByteArray) 
	{
		fileContents = new byte[aByteArray.length];
		System.arraycopy(aByteArray, 0, fileContents, 0, aByteArray.length);
	}
}
