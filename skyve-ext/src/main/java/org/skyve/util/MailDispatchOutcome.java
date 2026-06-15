package org.skyve.util;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Outcome metadata returned from {@link MailService} dispatch operations.
 * <p>
 * This type standardises provider-level status details so decorators (for example,
 * logging/auditing) can record dispatch results without depending on provider-specific APIs.
 * </p>
 */
public final class MailDispatchOutcome {

	/**
	 * Normalised dispatch state.
	 */
	public enum DispatchStatus {
		/**
		 * The message was accepted for delivery by the provider.
		 */
		SENT,
		/**
		 * The dispatch attempt failed.
		 */
		FAILED,
		/**
		 * Dispatch was intentionally skipped (for example, test/bogus-send mode).
		 */
		SKIPPED
	}

	private final @Nonnull DispatchStatus status;
	private final @Nullable String provider;
	private final @Nullable String providerMessageId;
	private final @Nullable String relayStatus;
	private final @Nullable String relayDetail;
	private final @Nullable String failureDetail;

	private MailDispatchOutcome(@Nonnull DispatchStatus status,
								@Nullable String provider,
								@Nullable String providerMessageId,
								@Nullable String relayStatus,
								@Nullable String relayDetail,
								@Nullable String failureDetail) {
		this.status = status;
		this.provider = provider;
		this.providerMessageId = providerMessageId;
		this.relayStatus = relayStatus;
		this.relayDetail = relayDetail;
		this.failureDetail = failureDetail;
	}

	public static @Nonnull MailDispatchOutcome sent(@Nullable String provider) {
		return new MailDispatchOutcome(DispatchStatus.SENT, provider, null, null, null, null);
	}

	public static @Nonnull MailDispatchOutcome sent(@Nullable String provider,
													@Nullable String providerMessageId,
													@Nullable String relayStatus,
													@Nullable String relayDetail) {
		return new MailDispatchOutcome(DispatchStatus.SENT, provider, providerMessageId, relayStatus, relayDetail, null);
	}

	public static @Nonnull MailDispatchOutcome failed(@Nullable String provider, @Nullable String failureDetail) {
		return new MailDispatchOutcome(DispatchStatus.FAILED, provider, null, null, null, failureDetail);
	}

	public static @Nonnull MailDispatchOutcome skipped(@Nullable String provider, @Nullable String relayDetail) {
		return new MailDispatchOutcome(DispatchStatus.SKIPPED, provider, null, "skipped", relayDetail, null);
	}

	public @Nonnull DispatchStatus getStatus() {
		return status;
	}

	public @Nullable String getProvider() {
		return provider;
	}

	public @Nullable String getProviderMessageId() {
		return providerMessageId;
	}

	public @Nullable String getRelayStatus() {
		return relayStatus;
	}

	public @Nullable String getRelayDetail() {
		return relayDetail;
	}

	public @Nullable String getFailureDetail() {
		return failureDetail;
	}
}
