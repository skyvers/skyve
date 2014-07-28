package org.skyve.wildcat.web.upload;

import java.util.Map;
import java.util.TreeMap;

public class UploadMonitor 
{
	private static Map<String, UploadInfo> uploadTable = new TreeMap<>();

	public static void set(String fName, UploadInfo info) 
	{
		uploadTable.put(fName, info);
	}

	public static void remove(String fName) 
	{
		uploadTable.remove(fName);
	}

	public static UploadInfo getInfo(String fName) 
	{
		return uploadTable.get(fName);
	}
}
