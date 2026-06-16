package org.skyve.impl.web.faces.views;

import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.apache.commons.io.FilenameUtils;
import org.primefaces.PrimeFaces;
import org.primefaces.event.FileUploadEvent;
import org.primefaces.model.file.UploadedFile;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.SecurityException;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.UploadException.Problem;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.view.widget.bound.input.ContentCapture;
import org.skyve.impl.metadata.view.widget.bound.input.ContentDisplay;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.WebErrorUtil;
import org.skyve.impl.web.content.ContentMediaClassifier;
import org.skyve.impl.web.content.ContentMediaClassifier.ContentMediaKind;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.views.UnifiedUploadState.UploadAffordance;
import org.skyve.impl.web.faces.views.UnifiedUploadState.UploadCategory;
import org.skyve.impl.web.faces.views.UnifiedUploadState.UploadKind;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.metadata.controller.WebFileInputStream;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.persistence.Persistence;
import org.skyve.util.OWASP;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.PostConstruct;
import jakarta.enterprise.context.RequestScoped;
import jakarta.faces.annotation.ManagedProperty;
import jakarta.faces.application.FacesMessage;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import jakarta.servlet.http.HttpServletRequest;

/**
 * Models a view interaction and binds it to the active Skyve web context.
 */
@RequestScoped
@Named("_skyveContent")
@SuppressWarnings("java:S1192") // Repeated literals are deliberate upload view response fragments.
public class ContentUploadView extends AbstractUploadView {
	private static final long serialVersionUID = -6769960348990922565L;

	private static final String FAILURE_MESSAGE_SUMMARY = "Failure";

	@Inject
	@ManagedProperty(value = "#{param." + AbstractWebContext.RESOURCE_FILE_NAME + "}")
	@SuppressWarnings("java:S6813") // allow member injection
	private @Nullable String contentBinding;

	@Inject
	@ManagedProperty(value = "#{param." + AbstractWebContext.ACTION_NAME + "}")
	@SuppressWarnings("java:S6813") // allow member injection
	private @Nullable String action;

	@Inject
	@ManagedProperty(value = "#{param._m}")
	@SuppressWarnings("java:S6813") // allow member injection
	private @Nullable String companionBinding;

	@Inject
	@ManagedProperty(value = "#{param._u}")
	@SuppressWarnings("java:S6813") // allow member injection
	private @Nullable String uploadKind;

	@Inject
	@ManagedProperty(value = "#{param._d}")
	@SuppressWarnings("java:S6813") // allow member injection
	private @Nullable String display;

	@Inject
	@ManagedProperty(value = "#{param._cap}")
	@SuppressWarnings("java:S6813") // allow member injection
	private @Nullable String capture;

	@Inject
	@ManagedProperty(value = "#{param._af}")
	@SuppressWarnings("java:S6813") // allow member injection
	private @Nullable String uploadAffordance;

	// Request state is nullable until JSF has injected parameters and @PostConstruct has parsed them.
	private @Nullable UnifiedUploadState uploadState;
	private @Nullable UploadAffordance activeUploadAffordance;

	/**
	 * Creates the request-scoped content upload view with framework content upload limits.
	 */
	public ContentUploadView() {
		super(UtilImpl.UPLOADS_CONTENT_WHITELIST_REGEX, UtilImpl.UPLOADS_CONTENT_MAXIMUM_SIZE_IN_MB);
	}

	/**
	 * Creates a content upload view with explicit whitelist and size constraints.
	 *
	 * @param whitelistRegex case-insensitive regex used to validate file names; may be {@code null}
	 * @param maximumSizeMB maximum allowed upload size in megabytes
	 */
	protected ContentUploadView(@Nullable String whitelistRegex, int maximumSizeMB) {
		super(whitelistRegex, maximumSizeMB);
	}

	@Override
	@PostConstruct
	public void postConstruct() {
		super.postConstruct();
		contentBinding = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(contentBinding));
		action = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(action));
		companionBinding = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(companionBinding));
		uploadKind = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(uploadKind));
		display = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(display));
		capture = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(capture));
		uploadAffordance = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(uploadAffordance));
		UnifiedUploadState state = UnifiedUploadState.fromRoute(
				(uploadKind == null) ? UploadKind.boundContent.name() : uploadKind,
				getContext(),
				getBinding(),
				contentBinding,
				action,
				(display == null) ? defaultDisplay().name() : display,
				(capture == null) ? defaultCapture().name() : capture,
				null,
				companionBinding,
				null);
		UploadAffordance affordance = resolveActiveUploadAffordance(state, uploadAffordance);
		uploadState = state;
		activeUploadAffordance = affordance;
		uploadAffordance = affordance.name();
		applyUploadLimits(state.categoryFor(affordance));
	}

	/**
	 * Restores cached upload conversation state before the page is rendered.
	 */
	public void preRender() {
		new FacesAction<Void>() {
			@Override
			public Void callback() throws Exception {
				initialise();
				return null;
			}
		}.execute();
	}

	/**
	 * Returns the binding that stores the uploaded content identifier.
	 *
	 * @return sanitised content binding, or {@code null} before post construction or for malformed routes
	 */
	public @Nullable String getContentBinding() {
		return contentBinding;
	}

	/**
	 * Returns the upload action name requested by an action-upload route.
	 *
	 * @return sanitised action name, or {@code null} for bound content uploads
	 */
	public @Nullable String getAction() {
		return action;
	}

	private final List<Problem> problems = new ArrayList<>();

	/**
	 * Returns action-upload problems accumulated during processing.
	 *
	 * @return collected upload problems
	 */
	public @Nonnull List<Problem> getProblems() {
		return problems;
	}

	/**
	 * Returns the validated unified upload state for the current request.
	 *
	 * @return upload state, or {@code null} before JSF post construction
	 */
	public @Nullable UnifiedUploadState getUploadState() {
		return uploadState;
	}

	/**
	 * Returns the selected upload affordance represented by the current request.
	 *
	 * <p>The value is posted by {@code upload.xhtml} as {@code _af} when a route
	 * exposes more than one affordance, for example {@code capture="all"}. The
	 * value is validated against the metadata-derived route state during
	 * {@link #postConstruct()} before it is used to select upload limits.
	 *
	 * @return selected upload affordance value for the hidden form field, or {@code null} before post construction
	 */
	public @Nullable String getUploadAffordance() {
		return uploadAffordance;
	}

	/**
	 * Returns the resolved display mode for the bound content upload route.
	 *
	 * @return resolved display mode
	 */
	public @Nonnull ContentDisplay getDisplay() {
		return (uploadState == null) ? ContentDisplay.auto : uploadState.getDisplay();
	}

	/**
	 * Returns the resolved capture mode for the bound content upload route.
	 *
	 * @return resolved capture mode
	 */
	public @Nonnull ContentCapture getCapture() {
		return (uploadState == null) ? ContentCapture.none : uploadState.getCapture();
	}

	/**
	 * Indicates whether this route uses image-specific upload controls.
	 *
	 * @return {@code true} when the route is image-capable
	 */
	public boolean isImageUpload() {
		return (uploadState != null) && UploadCategory.image.equals(uploadState.categoryFor(getActiveUploadAffordance()));
	}

	/**
	 * Indicates whether this route uses video-specific upload controls.
	 *
	 * @return {@code true} when the route is video-capable
	 */
	public boolean isVideoUpload() {
		return (uploadState != null) && UploadCategory.video.equals(uploadState.categoryFor(getActiveUploadAffordance()));
	}

	/**
	 * Indicates whether the normal choose/upload control should be shown.
	 *
	 * @return {@code true} after post construction when generic file upload is an allowed affordance
	 */
	public boolean isGenericUploadAllowed() {
		return isAffordanceAllowed(UploadAffordance.generic);
	}

	/**
	 * Indicates whether the camera/photo capture affordance should be shown.
	 *
	 * @return {@code true} after post construction when camera capture is an allowed affordance
	 */
	public boolean isCameraUploadAllowed() {
		return isAffordanceAllowed(UploadAffordance.camera);
	}

	/**
	 * Indicates whether the video capture affordance should be shown.
	 *
	 * @return {@code true} after post construction when video capture is an allowed affordance
	 */
	public boolean isVideoUploadAllowed() {
		return isAffordanceAllowed(UploadAffordance.video);
	}

	/**
	 * Returns the HTML accept hint for the active upload affordance.
	 *
	 * @return {@code image/*} for image/camera routes, {@code video/*} for video routes, or {@code null} for generic managed-content uploads
	 */
	public @Nullable String getAccept() {
		UploadAffordance affordance = getActiveUploadAffordance();
		if (UploadAffordance.camera.equals(affordance)) {
			return "image/*";
		}
		if (UploadAffordance.video.equals(affordance)) {
			return "video/*";
		}
		if ((uploadState != null) && UploadCategory.image.equals(uploadState.categoryFor(affordance))) {
			return "image/*";
		}
		if ((uploadState != null) && UploadCategory.video.equals(uploadState.categoryFor(affordance))) {
			return "video/*";
		}
		return null;
	}

	/**
	 * Returns the HTML capture hint for the active upload affordance.
	 *
	 * @return {@code environment} for camera capture, {@code camcorder} for video capture, or {@code null} when the generic file picker is active
	 */
	public @Nullable String getCaptureHint() {
		UploadAffordance affordance = getActiveUploadAffordance();
		if (UploadAffordance.camera.equals(affordance)) {
			return "environment";
		}
		if (UploadAffordance.video.equals(affordance)) {
			return "camcorder";
		}
		return null;
	}

	/**
	 * Returns the image upload limit used by client-side camera capture compression.
	 *
	 * @return image upload maximum size in bytes
	 */
	@SuppressWarnings("static-method") // JSF EL resolves upload.xhtml bean properties through instance getters.
	public long getCameraMaximumSizeInBytes() {
		return UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB * MB_IN_BYTES;
	}

	/**
	 * Validates the uploaded file against the active upload route.
	 *
	 * <p>Side effects: delegates to the base upload validation, which may enqueue a
	 * Faces message. Adds a media-specific validation message when an image-capable
	 * or video-capable route receives content that cannot be classified as the
	 * route's required media kind.
	 *
	 * @param file uploaded file metadata and content wrapper; must not be {@code null}
	 * @param fc active Faces context used for validation messages; must not be {@code null}
	 * @return {@code true} when the file satisfies size, whitelist, and route media-kind checks
	 */
	@Override
	protected boolean validFile(@Nonnull UploadedFile file, @Nonnull FacesContext fc) {
		if (! super.validFile(file, fc)) {
			return false;
		}
		ContentMediaKind mediaKind = ContentMediaClassifier.classify(file.getContentType(), file.getFileName());
		if (isImageUpload() && (! ContentMediaKind.image.equals(mediaKind))) {
			LOGGER.warn("FileUpload - Filename {} is not an image upload", file.getFileName());
			FacesMessage msg = new FacesMessage("Failure", "Filename " + file.getFileName() + " is not an image");
			fc.addMessage(null, msg);
			return false;
		}
		if (isVideoUpload() && (! ContentMediaKind.video.equals(mediaKind))) {
			LOGGER.warn("FileUpload - Filename {} is not a video upload", file.getFileName());
			FacesMessage msg = new FacesMessage("Failure", "Filename " + file.getFileName() + " is not a video");
			fc.addMessage(null, msg);
			return false;
		}
		return true;
	}

	/**
	 * Returns the default display mode when legacy upload routes omit unified route
	 * metadata.
	 *
	 * @return default display mode
	 */
	@SuppressWarnings("static-method") // Legacy image wrapper overrides this route default.
	protected @Nonnull ContentDisplay defaultDisplay() {
		return ContentDisplay.auto;
	}

	/**
	 * Returns the default capture mode when legacy upload routes omit unified route
	 * metadata.
	 *
	 * @return default capture mode
	 */
	private static @Nonnull ContentCapture defaultCapture() {
		return ContentCapture.none;
	}

	/**
	 * Processes a PrimeFaces file upload event for a bound managed-content field.
	 *
	 * <p>Side effects: when access and validation pass, stores the content,
	 * refreshes the cached web conversation, and writes a browser callback script to
	 * update the opener.
	 *
	 * @param event PrimeFaces upload event; must not be {@code null}
	 * @throws Exception if file streaming or content persistence fails
	 */
	public void handleFileUpload(@Nonnull FileUploadEvent event) throws Exception {
		Objects.requireNonNull(event, "Upload event");
		if ((uploadState != null) && UploadKind.action.equals(uploadState.getUploadKind())) {
			handleActionFileUpload(event);
			return;
		}
		FacesContext fc = Objects.requireNonNull(FacesContext.getCurrentInstance(), "Faces context");
		UploadedFile file = Objects.requireNonNull(event.getFile(), "Uploaded file");
		if (! validFile(file, fc)) {
			return;
		}
		upload(file.getFileName(),
				file.getContent(),
				file.getContentType(),
				fc);
	}

	/**
	 * Stores uploaded content against the bound managed-content field.
	 *
	 * <p>Side effects: verifies content and document access, writes the content to
	 * the content repository, caches the updated web conversation, rolls back
	 * persistence on failure, and enqueues a Faces message for user-visible upload
	 * errors. This method deliberately avoids {@link FacesAction} so upload iframe
	 * errors render as growls instead of being swallowed by normal action handling.
	 *
	 * @param fileName uploaded file name; must not be {@code null} when the upload reaches content persistence
	 * @param fileContents uploaded bytes; must not be {@code null} when the upload reaches content persistence
	 * @param contentType uploaded MIME type, or {@code null} when the browser did not supply one
	 * @param fc active Faces context used for messages and request lookup; must not be {@code null}
	 * @throws Exception if content persistence or access checks fail before the method handles the error
	 */
	private void upload(@Nullable String fileName,
							@Nullable byte[] fileContents,
							@Nullable String contentType,
							@Nonnull FacesContext fc) throws Exception {
		// If there is no access, don't process the upload and return to allow the view to render the no access message
		if (! isCanAccess()) {
			return;
		}

		String context = getContext();
		if ((context == null) || (contentBinding == null)) {
			LOGGER.warn("FileUpload - Malformed URL on Upload Action - context or contentBinding is null");
			FacesMessage msg = new FacesMessage("Failure", "Malformed URL");
			fc.addMessage(null, msg);
			return;
		}

		ExternalContext ec = fc.getExternalContext();
		HttpServletRequest request = (HttpServletRequest) ec.getRequest();

		AbstractWebContext webContext = StateUtil.getCachedConversation(context, request);
		if (webContext == null) {
			LOGGER.warn("FileUpload - Malformed URL on Content Upload - context does not exist");
			FacesMessage msg = new FacesMessage("Failure", "Malformed URL");
			fc.addMessage(null, msg);
			return;
		}

		// NB Persistence has been set with the restore processing inside the SkyveFacesPhaseListener
		Persistence persistence = CORE.getPersistence();
		try {
			Bean currentBean = webContext.getCurrentBean();
			Bean bean = currentBean;

			String binding = getBinding();
			if (binding != null) {
				bean = (Bean) BindUtil.get(bean, binding);
			}
			if (bean == null) { // should never happen
				throw new IllegalStateException("bean is null");
			}

			// Check content access
			User user = persistence.getUser();
			String bizModule = bean.getBizModule();
			String bizDocument = bean.getBizDocument();
			UxUi uxui = UserAgent.getUxUi(request);
			String unsanitisedContentBinding = BindUtil.unsanitiseBinding(contentBinding);
			EXT.checkAccess(user, UserAccess.content(bizModule, bizDocument, unsanitisedContentBinding), uxui.getName());

			// Check document access
			Customer customer = user.getCustomer();
			Document document = customer.getModule(bizModule).getDocument(customer, bizDocument);
			if (! user.canAccessDocument(document)) {
				throw new SecurityException("view this document", user.getName());
			}

			// Add to content
			// NB This handles compound bindings and checks for content access on the content owning bean
			AttachmentContent content = FacesContentUtil.handleFileUpload(Objects.requireNonNull(fileName, "Uploaded file name"),
																			Objects.requireNonNull(fileContents, "Uploaded file content"),
																			contentType,
																			bean,
																			unsanitisedContentBinding);
			String contentId = Objects.requireNonNull(content.getContentId(), "Uploaded content id");

			// only put conversation in cache if we have been successful in executing
			StateUtil.cacheConversation(webContext);

			// update the content UUID value on the client and popoff the window on the stack
			String sanitisedContentBinding = Objects.requireNonNull(BindUtil.sanitiseBinding(contentBinding), "Sanitised content binding");
			PrimeFaces.current().executeScript(createUploadSuccessScript(bean, content, contentId, sanitisedContentBinding));
		}
		catch (Exception e) {
			persistence.rollback();
			String reference = WebErrorUtil.logUnexpectedAndGetReference(LOGGER, "Content upload failed for binding " + contentBinding, e);
			FacesMessage msg = new FacesMessage("Failure", WebErrorUtil.genericMessage(reference));
			fc.addMessage(null, msg);
		}
		// NB No need to disconnect Persistence as it is done in the SkyveFacesPhaseListener after the response is rendered.
	}

	/**
	 * Processes an uploaded file for an action route while preserving
	 * legacy action-upload semantics.
	 *
	 * <p>Side effects: executes the configured upload action, updates the cached
	 * conversation bean when the action returns one, rolls persistence back for
	 * upload errors, and enqueues user-facing JSF messages.
	 *
	 * @param event PrimeFaces upload event; must not be {@code null}
	 * @throws Exception if upload preconditions or processing fail unexpectedly
	 */
	@SuppressWarnings("java:S3776") // Mirrors legacy action-upload semantics.
	private void handleActionFileUpload(@Nonnull FileUploadEvent event) throws Exception {
		if (! isCanAccess()) {
			return;
		}

		FacesContext fc = Objects.requireNonNull(FacesContext.getCurrentInstance(), "Faces context");
		UploadedFile file = Objects.requireNonNull(event.getFile(), "Uploaded file");
		if (! validFile(file, fc)) {
			return;
		}

		String context = getContext();
		if ((context == null) || (action == null)) {
			LOGGER.warn("FileUpload - Malformed URL on Upload Action - context, binding, or action is null");
			FacesMessage msg = new FacesMessage(FAILURE_MESSAGE_SUMMARY, "Malformed URL");
			fc.addMessage(null, msg);
			return;
		}
		String actionName = Objects.requireNonNull(action, "Upload action");

		ExternalContext ec = fc.getExternalContext();
		HttpServletRequest request = (HttpServletRequest) ec.getRequest();

		AbstractWebContext webContext = StateUtil.getCachedConversation(context, request);
		if (webContext == null) {
			LOGGER.warn("FileUpload - Malformed URL on Upload Action - context does not exist");
			FacesMessage msg = new FacesMessage(FAILURE_MESSAGE_SUMMARY, "Malformed URL");
			fc.addMessage(null, msg);
			return;
		}

		Persistence persistence = CORE.getPersistence();
		try {
			User user = persistence.getUser();
			CustomerImpl customer = (CustomerImpl) user.getCustomer();

			Bean currentBean = webContext.getCurrentBean();

			Bean bean = currentBean;
			String binding = getBinding();
			if (binding != null) {
				bean = (Bean) BindUtil.get(bean, binding);
			}
			if (bean == null) {
				throw new IllegalStateException("bean is null");
			}

			Module module = customer.getModule(bean.getBizModule());
			Document document = module.getDocument(customer, bean.getBizDocument());

			if (! user.canExecuteAction(document, actionName)) {
				throw new SecurityException(actionName, user.getName());
			}
			UploadAction<Bean> uploadAction = document.getUploadAction(customer, actionName, true);
			UploadException exception = new UploadException();
			MimeType mimeType = null;
			try {
				mimeType = MimeType.valueOf(file.getContentType());
			}
			catch (@SuppressWarnings("unused") Exception e) {
				// do nothing
			}

			try (WebFileInputStream in = new WebFileInputStream(file.getInputStream())) {
				try {
					Upload upload = new Upload(FilenameUtils.getName(file.getFileName()), in, mimeType);
					boolean vetoed = customer.interceptBeforeUploadAction(document, actionName, bean, upload, webContext);
					if (! vetoed) {
						bean = uploadAction.upload(bean, upload, exception, webContext);
						if (binding == null) {
							webContext.setCurrentBean(bean);
						}
						else {
							BindUtil.set(currentBean, binding, bean);
						}

						customer.interceptAfterUploadAction(document, actionName, bean, upload, webContext);

						if (exception.hasErrors()) {
							throw exception;
						}
					}
				}
				finally {
					in.processed();
				}
			}
			catch (UploadException e) {
				LOGGER.warn("File upload completed with user-facing upload problems.", e);
				persistence.rollback();
				exception = e;
			}
			catch (IOException e) {
				throw new DomainException("File Upload could not be processed", e);
			}

			StateUtil.cacheConversation(webContext);

			if (exception.hasProblems()) {
				addActionUploadProblems(exception, fc);
			}
			else {
				addActionUploadSuccess(file, fc);
			}
		}
		catch (Exception e) {
			persistence.rollback();
			String reference = WebErrorUtil.logUnexpectedAndGetReference(LOGGER, "File upload failed for action " + actionName, e);
			FacesMessage msg = new FacesMessage(FAILURE_MESSAGE_SUMMARY, WebErrorUtil.genericMessage(reference));
			fc.addMessage(null, msg);
		}
	}

	/**
	 * Adds action-upload validation problems to the request model and queues the
	 * matching summary message.
	 *
	 * <p>Side effects: mutates {@link #problems} and adds a Faces message to the
	 * supplied context.
	 *
	 * @param exception upload exception containing user-facing errors and warnings; must not be {@code null}
	 * @param fc active Faces context used for messages; must not be {@code null}
	 */
	private void addActionUploadProblems(@Nonnull UploadException exception, @Nonnull FacesContext fc) {
		for (Problem error : exception.getErrors()) {
			problems.add(error);
		}
		for (Problem warning : exception.getWarnings()) {
			problems.add(warning);
		}

		if (exception.hasErrors()) {
			String message = "The import did <b>NOT</b> complete successfully.<br/>" +
								"No data has changed as a result of this import.<br/>" +
								"Please review the errors and warnings displayed before closing this window.<br/>" +
								"The above list includes only the first 50 errors and warnings, there may be more.<br/>" +
								"If the nature of the problem is not clear from the message, it may be because it is caused by another issue being compounded.<br/>" +
								"In this case, you may find that fixing one or two problems you can easily identify, may resolve a number of related issues.";
			FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_ERROR, "Unsuccessful", message);
			fc.addMessage(null, msg);
		}
		else {
			String message = "The import completed successfully with warnings.<br/>" +
								"Please review the warnings displayed before closing this window.<br/>" +
								"The above list includes only the first 50 errors and warnings, there may be more.<br/>" +
								"If the nature of the problem is not clear from the message, it may be because it is caused by another issue being compounded.<br/>" +
								"In this case, you may find that fixing one or two problems you can easily identify, may resolve a number of related issues.";
			FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_WARN, "Successful", message);
			fc.addMessage(null, msg);
		}
	}

	/**
	 * Queues a successful action-upload message including the uploaded file size.
	 *
	 * @param file uploaded file metadata; must not be {@code null}
	 * @param fc active Faces context used for messages; must not be {@code null}
	 */
	private static void addActionUploadSuccess(@Nonnull UploadedFile file, @Nonnull FacesContext fc) {
		long size = file.getSize();
		StringBuilder message = new StringBuilder(128);
		message.append(file.getFileName()).append(" is uploaded. File Size is ");

		DecimalFormat format = CORE.getDecimalFormat("###,##0.00");
		if (size > 1048576) {
			message.append(format.format(size / 1048576.0)).append(" MB");
		}
		else {
			message.append(format.format(size / 1024.0)).append(" KB");
		}

		FacesMessage msg = new FacesMessage("Successful", message.toString());
		fc.addMessage(null, msg);
	}

	/**
	 * Builds the browser callback script that updates content controls after a successful upload.
	 *
	 * @param bean bean that owns the content field; must not be {@code null}
	 * @param content stored attachment metadata; must not be {@code null}
	 * @param contentId stored content identifier; must not be {@code null}
	 * @param sanitisedContentBinding client-side binding name to update; must not be {@code null}
	 * @return JavaScript callback text for SmartClient or PrimeFaces callers; never {@code null}
	 */
	@Nonnull String createUploadSuccessScript(@Nonnull Bean bean,
												@Nonnull AttachmentContent content,
												@Nonnull String contentId,
												@Nonnull String sanitisedContentBinding) {
		StringBuilder js = new StringBuilder(256);
		String mediaKind = ContentMediaClassifier.classify(content.getContentType(), content.getFileName()).name();
		// if window.parent.isc is defined then we are using smart client, set the value in the values manager
		js.append("if(window.parent.isc){");
		js.append("if(window.parent.isc.BizUtil&&window.parent.isc.BizUtil.afterContentUpload){");
		appendSmartClientUploadCallback(js, bean, content, contentId, sanitisedContentBinding, mediaKind);
		js.append("}else{");
		if (companionBinding != null) {
			js.append("window.parent.isc.WindowStack.getOpener()._vm.setValue('");
			js.append(OWASP.escapeJsString(companionBinding, false, false));
			js.append("','").append(mediaKind).append("');");
		}
		js.append("window.parent.isc.WindowStack.getOpener()._vm.setValue('").append(sanitisedContentBinding);
		js.append("','").append(contentId).append("');");
		js.append("window.parent.isc.WindowStack.popoff(false)");
		js.append('}');
		// otherwise we are using prime faces, set the hidden input element that ends with "_<binding>"
		// NB Cannot use window.parent here to support nested frames as the script is executed at the top window context.
		js.append("}else if(top.SKYVE){if(top.SKYVE.PF){top.SKYVE.PF.afterContentUpload('").append(sanitisedContentBinding);
		js.append("','").append(contentId).append("','");
		js.append(bean.getBizModule()).append('.').append(bean.getBizDocument()).append("','");
		js.append(OWASP.escapeJsString(content.getFileName(), false, false)).append("','");
		js.append(mediaKind).append('\'');
		if (companionBinding != null) {
			js.append(",'").append(OWASP.escapeJsString(companionBinding, false, false)).append('\'');
		}
		js.append(")}}");
		return js.toString();
	}

	private void appendSmartClientUploadCallback(@Nonnull StringBuilder js,
													@Nonnull Bean bean,
													@Nonnull AttachmentContent content,
													@Nonnull String contentId,
													@Nonnull String sanitisedContentBinding,
													@Nonnull String mediaKind) {
		js.append("window.parent.isc.BizUtil.afterContentUpload('").append(sanitisedContentBinding);
		js.append("','").append(contentId).append("','");
		js.append(bean.getBizModule()).append('.').append(bean.getBizDocument()).append("','");
		js.append(OWASP.escapeJsString(content.getFileName(), false, false)).append("','");
		js.append(mediaKind).append('\'');
		if (companionBinding != null) {
			js.append(",'").append(OWASP.escapeJsString(companionBinding, false, false)).append('\'');
		}
		js.append(");");
	}

	/**
	 * Applies filename whitelist and maximum-size limits for the selected upload
	 * category.
	 *
	 * <p>Side effects: mutates the request-scoped upload validation limits used by
	 * {@link #validFile(UploadedFile, FacesContext)}.
	 *
	 * @param category selected upload category; must not be {@code null}
	 */
	private void applyUploadLimits(@Nonnull UploadCategory category) {
		if (UploadCategory.image.equals(category)) {
			setUploadLimits(UtilImpl.UPLOADS_IMAGE_WHITELIST_REGEX, UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB);
		}
		else if (UploadCategory.video.equals(category)) {
			setUploadLimits(UtilImpl.UPLOADS_VIDEO_WHITELIST_REGEX, UtilImpl.UPLOADS_VIDEO_MAXIMUM_SIZE_IN_MB);
		}
		else if (UploadCategory.file.equals(category)) {
			setUploadLimits(UtilImpl.UPLOADS_FILE_WHITELIST_REGEX, UtilImpl.UPLOADS_FILE_MAXIMUM_SIZE_IN_MB);
		}
		else {
			setUploadLimits(UtilImpl.UPLOADS_CONTENT_WHITELIST_REGEX, UtilImpl.UPLOADS_CONTENT_MAXIMUM_SIZE_IN_MB);
		}
	}

	/**
	 * Returns the route affordance currently represented by the file upload control.
	 *
	 * @return selected upload affordance, or {@link UploadAffordance#generic} before post construction
	 */
	private @Nonnull UploadAffordance getActiveUploadAffordance() {
		return (activeUploadAffordance == null) ? UploadAffordance.generic : activeUploadAffordance;
	}

	/**
	 * Indicates whether an upload affordance is available for the current route.
	 *
	 * @param affordance upload affordance to test
	 * @return {@code true} after post construction when the affordance should be rendered
	 */
	private boolean isAffordanceAllowed(@Nonnull UploadAffordance affordance) {
		return (uploadState != null) && uploadState.isAffordanceAllowed(affordance);
	}

	/**
	 * Returns the upload affordance that determines the initial validation bucket for
	 * a route.
	 *
	 * <p>For capture-only routes, the selected capture affordance controls the
	 * upload category even when no generic chooser is allowed. For normal and
	 * {@code capture="all"} routes, the generic affordance is the initial bucket;
	 * later capture-specific controls choose their own bucket.
	 *
	 * @param state validated unified upload route state; must not be {@code null}
	 * @return primary upload affordance; never {@code null}
	 */
	private static @Nonnull UploadAffordance primaryUploadAffordance(@Nonnull UnifiedUploadState state) {
		ContentCapture resolvedCapture = state.getCapture();
		if (ContentCapture.camera.equals(resolvedCapture)) {
			return UploadAffordance.camera;
		}
		if (ContentCapture.video.equals(resolvedCapture)) {
			return UploadAffordance.video;
		}
		return UploadAffordance.generic;
	}

	/**
	 * Resolves the upload affordance selected by the user for this request.
	 *
	 * <p>Precondition: {@code uploadAffordanceValue}, when supplied, must be one of
	 * {@link UploadAffordance}. The parsed value must also be allowed by
	 * {@code state}; otherwise the route is rejected before upload processing.
	 *
	 * @param state validated route state
	 * @param uploadAffordanceValue optional posted affordance value
	 * @return selected affordance
	 * @throws IllegalArgumentException when the posted affordance is invalid or not allowed by {@code state}
	 */
	private static @Nonnull UploadAffordance resolveActiveUploadAffordance(@Nonnull UnifiedUploadState state,
																			@Nullable String uploadAffordanceValue) {
		UploadAffordance result = primaryUploadAffordance(state);
		if (uploadAffordanceValue != null) {
			try {
				result = UploadAffordance.valueOf(uploadAffordanceValue);
			}
			catch (IllegalArgumentException e) {
				throw new IllegalArgumentException("Invalid upload affordance value: " + uploadAffordanceValue, e);
			}
		}
		if (! state.isAffordanceAllowed(result)) {
			throw new IllegalArgumentException("Upload affordance " + result + " is not allowed for " +
												state.getDisplay() + " / " + state.getCapture() + ".");
		}
		return result;
	}
}
