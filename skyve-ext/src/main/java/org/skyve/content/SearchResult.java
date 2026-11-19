package org.skyve.content;

import java.io.Serializable;
import java.util.Date;

import jakarta.annotation.Nullable;

/**
 * A single search result for either a bean or attachment content.
 * @author mike
 */
public final class SearchResult implements Serializable {
	private static final long serialVersionUID = -5096781261044743153L;

	private String contentId;
	private String excerpt;
	private int score;
	private String customerName;
	private String moduleName;
	private String documentName;
	private String bizDataGroupId;
	private String bizUserId;
	private String bizId;
	private String attributeName;
	private Date lastModified;

	/**
	 * Is this search result a hit on an attachment?
	 * @return
	 */
	public boolean isAttachment() {
		return (attributeName != null);
	}
	
	/**
	 * From the attachment
	 * @return
	 */
	public @Nullable String getContentId() {
		return contentId;
	}

	/**
	 * From the attachment
	 * @param contentId
	 */
	public void setContentId(@Nullable String contentId) {
		this.contentId = contentId;
	}

	/**
	 * A highlighted snippet of the match within the text.
	 * @return
	 */
	public @Nullable String getExcerpt() {
		return excerpt;
	}

	/**
	 * A highlighted snippet of the match within the text.
	 * @param excerpt
	 */
	public void setExcerpt(@Nullable String excerpt) {
		this.excerpt = excerpt;
	}

	/**
	 * The search relevance score.
	 * @return
	 */
	public int getScore() {
		return score;
	}

	/**
	 * The search relevance score.
	 * @param score
	 */
	public void setScore(int score) {
		this.score = score;
	}

	/**
	 * From the content
	 * @return
	 */
	public @Nullable String getCustomerName() {
		return customerName;
	}

	/**
	 * From the content
	 * @param customerName
	 */
	public void setCustomerName(@Nullable String customerName) {
		this.customerName = customerName;
	}

	/**
	 * From the content
	 * @return
	 */
	public @Nullable String getModuleName() {
		return moduleName;
	}

	/**
	 * From the content
	 * @param moduleName
	 */
	public void setModuleName(@Nullable String moduleName) {
		this.moduleName = moduleName;
	}

	/**
	 * From the content
	 * @return
	 */
	public @Nullable String getDocumentName() {
		return documentName;
	}

	/**
	 * From the content
	 * @param documentName
	 */
	public void setDocumentName(@Nullable String documentName) {
		this.documentName = documentName;
	}

	/**
	 * From the content
	 * @return
	 */
	public @Nullable String getBizDataGroupId() {
		return bizDataGroupId;
	}

	/**
	 * From the content
	 * @param bizDataGroupId
	 */
	public void setBizDataGroupId(@Nullable String bizDataGroupId) {
		this.bizDataGroupId = bizDataGroupId;
	}

	/**
	 * From the content
	 * @return
	 */
	public @Nullable String getBizUserId() {
		return bizUserId;
	}

	/**
	 * From the content
	 * @param bizUserId
	 */
	public void setBizUserId(@Nullable String bizUserId) {
		this.bizUserId = bizUserId;
	}

	/**
	 * From the content
	 * @return
	 */
	public @Nullable String getBizId() {
		return bizId;
	}

	/**
	 * From the content
	 * @param bizId
	 */
	public void setBizId(@Nullable String bizId) {
		this.bizId = bizId;
	}

	/**
	 * From the attachment
	 * @return
	 */
	public @Nullable String getAttributeName() {
		return attributeName;
	}

	/**
	 * From the attachment
	 * @param attributeName
	 */
	public void setAttributeName(@Nullable String attributeName) {
		this.attributeName = attributeName;
	}

	/**
	 * From the attachment
	 * @return
	 */
	public @Nullable Date getLastModified() {
		return lastModified;
	}

	/**
	 * From the attachment
	 * @param lastModified
	 */
	public void setLastModified(@Nullable Date lastModified) {
		this.lastModified = lastModified;
	}
}
