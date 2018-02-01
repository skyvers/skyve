package org.skyve.impl.content.ejb;

import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.SearchResults;

public interface EJBRemoteContentManagerServer {
	public void put(BeanContent content) throws Exception;

	/**
	 * 
	 * @param content The AttachmentContent to put.
	 * @param index	whether to index of not
	 * @return	The contentId.
	 * @throws Exception
	 */
	public String put(AttachmentContent content, boolean index) throws Exception;

	public AttachmentContent get(String id) throws Exception;
	
	public void remove(BeanContent content) throws Exception;
	
	public void remove(String contentId) throws Exception;
	
	public SearchResults google(String search, int maxResults) throws Exception;
}
