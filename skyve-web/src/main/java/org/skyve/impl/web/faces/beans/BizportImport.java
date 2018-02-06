package org.skyve.impl.web.faces.beans;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import javax.faces.application.FacesMessage;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ManagedProperty;
import javax.faces.bean.RequestScoped;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.primefaces.event.FileUploadEvent;
import org.primefaces.model.UploadedFile;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.UploadException.Problem;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.bizport.POIWorkbook;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.WebUtil;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.metadata.controller.BizImportAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;

@ManagedBean(name = "_skyveBizImport")
@RequestScoped
public class BizportImport extends Localisable {
	private static final long serialVersionUID = -8452779436908784172L;

	@ManagedProperty(value = "#{param." + AbstractWebContext.CONTEXT_NAME + "}")
    private String context;
    
    @ManagedProperty(value = "#{param." + AbstractWebContext.BINDING_NAME + "}")
    private String binding;

    @ManagedProperty(value = "#{param." + AbstractWebContext.ACTION_NAME + "}")
    private String action;

	public void preRender() {
		new FacesAction<Void>() {
			@Override
			public Void callback() throws Exception {
				Persistence p = CORE.getPersistence();
				UserImpl internalUser = (UserImpl) p.getUser();
				initialise(internalUser, FacesContext.getCurrentInstance().getExternalContext().getRequestLocale());
				
				return null;
			}
		}.execute();
	}
	
    public String getContext() {
		return context;
	}

	public void setContext(String context) {
		this.context = UtilImpl.processStringValue(context);
	}

	public String getBinding() {
		return binding;
	}

	public void setBinding(String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}

	public String getAction() {
		return action;
	}

	public void setAction(String action) {
		this.action = UtilImpl.processStringValue(action);
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
	public void handleFileUpload(FileUploadEvent event)
	throws Exception {
		FacesContext fc = FacesContext.getCurrentInstance();

		if ((context == null) || (action == null)) {
			UtilImpl.LOGGER.warning("FileUpload - Malformed URL on Upload Action - context, binding, or action is null");
			FacesMessage msg = new FacesMessage("Failure", "Malformed URL");
	        fc.addMessage(null, msg);
	        return;
		}

		ExternalContext ec = fc.getExternalContext();
		HttpServletRequest request = (HttpServletRequest) ec.getRequest();
		HttpServletResponse response = (HttpServletResponse) ec.getResponse();

		UploadedFile file = event.getFile();
		
		AbstractWebContext webContext = WebUtil.getCachedConversation(context, request, response);
		if (webContext == null) {
			UtilImpl.LOGGER.warning("FileUpload - Malformed URL on Upload Action - context does not exist");
			FacesMessage msg = new FacesMessage("Failure", "Malformed URL");
	        FacesContext.getCurrentInstance().addMessage(null, msg);
	        return;
		}

		// NB Persistence has been set with the restore processing inside the SkyvePhaseListener
		Persistence persistence = CORE.getPersistence();
		try {
			AbstractRepository repository = AbstractRepository.get();
			User user = persistence.getUser();
			Customer customer = user.getCustomer();
		
			Bean currentBean = webContext.getCurrentBean();
			Bean bean = currentBean;

			if (binding != null) {
				bean = (Bean) BindUtil.get(bean, binding);
			}

			Module module = customer.getModule(bean.getBizModule());
			Document document = module.getDocument(customer, bean.getBizDocument());
			
			if (! user.canExecuteAction(document, action)) {
				throw new SecurityException(action, user.getName());
			}
			BizImportAction bizPortAction = repository.getBizImportAction(customer, 
																			document,
																			action,
																			true);

			UploadException exception = new UploadException();
			try {
				try (InputStream fis = file.getInputstream()) {
					POIWorkbook workbook = new POIWorkbook(customer,
															WorkbookFactory.create(fis), 
															exception);
					CustomerImpl internalCustomer = (CustomerImpl) customer;
					boolean vetoed = internalCustomer.interceptBeforeBizImportAction(document, action, workbook, exception);
					if (! vetoed) {
						bizPortAction.bizImport(workbook, exception);
						internalCustomer.interceptAfterBizImportAction(document, action, workbook, exception);
					}
		
					// throw if we have errors found, to ensure rollback
					if (exception.hasErrors()) {
						throw exception;
					}
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
			WebUtil.putConversationInCache(webContext);

			
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
		        FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_INFO, "Successful", "The import completed successfully.");
		        fc.addMessage(null, msg);
			}
		}
		catch (Throwable t) {
			persistence.rollback();
			t.printStackTrace();
			FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_ERROR, "Failure", t.getMessage());
	        fc.addMessage(null, msg);
		}
		// NB No need to disconnect Persistence as it is done in the SkyvePhaseListener after the response is rendered.
	}
}
