package org.skyve.impl.web.faces.views;

import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.primefaces.event.FileUploadEvent;
import org.primefaces.model.file.UploadedFile;
import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.SecurityException;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.UploadException.Problem;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.metadata.controller.WebFileInputStream;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.persistence.Persistence;
import org.skyve.util.OWASP;

import jakarta.annotation.PostConstruct;
import jakarta.enterprise.context.RequestScoped;
import jakarta.faces.annotation.ManagedProperty;
import jakarta.faces.application.FacesMessage;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@RequestScoped
@Named("_skyveUpload")
public class FileUploadView extends AbstractUploadView {
	private static final long serialVersionUID = -8705052124876109265L;

	@Inject
	@ManagedProperty(value = "#{param." + AbstractWebContext.ACTION_NAME + "}")
	private String action;

	public FileUploadView() {
		super(UtilImpl.UPLOADS_FILE_WHITELIST_REGEX, UtilImpl.UPLOADS_FILE_MAXIMUM_SIZE_IN_MB);
	}

	@Override
	@PostConstruct
	public void postConstruct() {
		super.postConstruct();
		action = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(action));
	}

	public void preRender() {
		new FacesAction<Void>() {
			@Override
			public Void callback() throws Exception {
				initialise();
				return null;
			}
		}.execute();
	}

	public String getAction() {
		return action;
	}

	private List<Problem> problems = new ArrayList<>();

	public List<Problem> getProblems() {
		return problems;
	}

	/**
	 * Process the file upload
	 * 
	 * @param event
	 */
	public void handleFileUpload(FileUploadEvent event) throws Exception {
		FacesContext fc = FacesContext.getCurrentInstance();

		UploadedFile file = event.getFile();
		if (! validFile(file, fc)) {
			return;
		}

		String context = getContext();
		if ((context == null) || (action == null)) {
			UtilImpl.LOGGER.warning("FileUpload - Malformed URL on Upload Action - context, binding, or action is null");
			FacesMessage msg = new FacesMessage("Failure", "Malformed URL");
			fc.addMessage(null, msg);
			return;
		}

		ExternalContext ec = fc.getExternalContext();
		HttpServletRequest request = (HttpServletRequest) ec.getRequest();
		HttpServletResponse response = (HttpServletResponse) ec.getResponse();

		AbstractWebContext webContext = StateUtil.getCachedConversation(context, request, response);
		if (webContext == null) {
			UtilImpl.LOGGER.warning("FileUpload - Malformed URL on Upload Action - context does not exist");
			FacesMessage msg = new FacesMessage("Failure", "Malformed URL");
			FacesContext.getCurrentInstance().addMessage(null, msg);
			return;
		}

		// NB Persistence has been set with the restore processing inside the SkyveFacesPhaseListener
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
			if (bean == null) { // should never happen
				throw new IllegalStateException("bean is null");
			}

			Module module = customer.getModule(bean.getBizModule());
			Document document = module.getDocument(customer, bean.getBizDocument());

			if (! user.canExecuteAction(document, action)) {
				throw new SecurityException(action, user.getName());
			}
			UploadAction<Bean> uploadAction = document.getUploadAction(customer, action, true);
			UploadException exception = new UploadException();
			MimeType mimeType = null;
			try {
				MimeType.valueOf(file.getContentType());
			}
			catch (@SuppressWarnings("unused") Exception e) {
				// do nothing
			}

			try (WebFileInputStream in = new WebFileInputStream(file.getInputStream())) {
				try {
					Upload upload = new Upload(FilenameUtils.getName(file.getFileName()), in, mimeType);
					boolean vetoed = customer.interceptBeforeUploadAction(document, action, bean, upload, webContext);
					if (! vetoed) {
						bean = uploadAction.upload(bean, upload, exception, webContext);
						if (binding == null) {
							webContext.setCurrentBean(bean);
						}
						else {
							BindUtil.set(currentBean, binding, bean);
						}

						customer.interceptAfterUploadAction(document, action, bean, upload, webContext);

						// throw if we have errors found, to ensure rollback
						if (exception.hasErrors()) {
							throw exception;
						}
					}
				}
				finally {
					in.processed(); // ensure close is called on the in stream here
				}
			}
			catch (UploadException e) {
				e.printStackTrace();
				persistence.rollback();
				exception = e;
			}
			catch (IOException e) { // hide any file system paths from file operation problems encountered
				throw new DomainException("File Upload could not be processed", e);
			}

			// only put conversation in cache if we have been successful in executing
			StateUtil.cacheConversation(webContext);

			if (exception.hasProblems()) {
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
			else {
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
				FacesContext.getCurrentInstance().addMessage(null, msg);
			}
		}
		catch (Exception e) {
			persistence.rollback();
			e.printStackTrace();
			FacesMessage msg = new FacesMessage("Failure", e.getMessage());
			fc.addMessage(null, msg);
		}
		// NB No need to disconnect Persistence as it is done in the SkyveFacesPhaseListener after the response is rendered.
	}
}
