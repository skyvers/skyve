package org.skyve.wildcat.web.upload;

public class UploadInfo 
{
	public long totalSize;
	public long currSize;
	public long starttime;
	public boolean aborted;

	public UploadInfo() 
	{
		totalSize = 0l;
		currSize = 0l;
		starttime = System.currentTimeMillis();
		aborted = false;
	}

	public UploadInfo(int size) 
	{
		totalSize = size;
		currSize = 0;
		starttime = System.currentTimeMillis();
		aborted = false;
	}

	public String getUploadRate() 
	{
		long time = System.currentTimeMillis() - starttime;
		if (time != 0) {
			long uprate = currSize * 1000 / time;
			return AbstractUploadServlet.convertFileSize(uprate) + "/s";
		}
		return "n/a";
	}

	public int getPercent() 
	{
		if (totalSize == 0) return 0;
		return (int) (currSize * 100 / totalSize);
	}

	public String getTimeElapsed() 
	{
		long time = (System.currentTimeMillis() - starttime) / 1000l;
		if (time - 60l >= 0){
			if (time % 60 >=10) return time / 60 + ":" + (time % 60) + "m";
			return time / 60 + ":0" + (time % 60) + "m";
		}
		return time<10 ? "0" + time + "s": time + "s";
	}

	public String getTimeEstimated() 
	{
		if (currSize == 0) return "n/a";
		long time = System.currentTimeMillis() - starttime;
		time = totalSize * time / currSize;
		time /= 1000l;
		if (time - 60l >= 0){
			if (time % 60 >=10) return time / 60 + ":" + (time % 60) + "m";
			return time / 60 + ":0" + (time % 60) + "m";
		}
		return time<10 ? "0" + time + "s": time + "s";
	}
}
