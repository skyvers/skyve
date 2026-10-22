package org.skyve.content;

import java.io.Serializable;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Content.
 * 
 * @author Mike
 */
abstract class Content implements Serializable {
	private static final long serialVersionUID = -4098739421963456076L;

	protected String bizCustomer;
	protected String bizModule;
	protected String bizDocument;
	protected String bizDataGroupId;
	protected String bizUserId;
	protected String bizId;

	protected Content(@Nullable String bizCustomer, 
						@Nonnull String bizModule,
						@Nonnull String bizDocument,
						@Nullable String bizDataGroupId,
						@Nullable String bizUserId,
						@Nonnull String bizId) {
		this.bizCustomer = bizCustomer;
		this.bizModule = bizModule;
		this.bizDocument = bizDocument;
		this.bizDataGroupId = bizDataGroupId;
		this.bizUserId = bizUserId;
		this.bizId = bizId;
	}

	/**
	 * Get the bizCustomer for the data.
	 * This is used to restrict visibility of searched documents.
	 * @return
	 */
	public final @Nullable String getBizCustomer() {
		return bizCustomer;
	}

	/**
	 * Get the bizDataGroup for the data.
	 * This is used to restrict visibility of searched documents.
	 * @return
	 */
	public final @Nullable String getBizDataGroupId() {
		return bizDataGroupId;
	}

	/**
	 * Get the bizDocument for the data.
	 * This is used to restrict visibility of searched documents.
	 * @return
	 */
	public final @Nonnull String getBizDocument() {
		return bizDocument;
	}

	/**
	 * Get the bizModule for the data.
	 * This is used to restrict visibility of searched documents.
	 * @return
	 */
	public final @Nonnull String getBizModule() {
		return bizModule;
	}

	/**
	 * Get the bizUserId for the data.
	 * This is used to restrict visibility of searched documents.
	 * @return
	 */
	public final @Nullable String getBizUserId() {
		return bizUserId;
	}

	/**
	 * The bizId of the related bean.
	 * @return
	 */
	public final @Nonnull String getBizId() {
		return bizId;
	}
}
