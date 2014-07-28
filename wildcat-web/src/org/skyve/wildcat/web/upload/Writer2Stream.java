package org.skyve.wildcat.web.upload;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;

/**
 * Wrapperclass to wrap an OutputStream around a Writer
 */
class Writer2Stream extends OutputStream 
{
	Writer out;

	Writer2Stream(Writer w) 
	{
		super();
		out = w;
	}

	@Override
	public void write(int i) 
	throws IOException 
	{
		out.write(i);
	}

	@Override
	public void write(byte[] b) 
	throws IOException
	{
		for (int i = 0; i < b.length; i++) 
		{
			int n = b[i];
			//Convert byte to ubyte
			n = ((n >>> 4) & 0xF) * 16 + (n & 0xF);
			out.write(n);
		}
	}

	@Override
	public void write(byte[] b, int off, int len) 
	throws IOException 
	{
		for (int i = off; i < off + len; i++) 
		{
			int n = b[i];
			n = ((n >>> 4) & 0xF) * 16 + (n & 0xF);
			out.write(n);
		}
	}
}
