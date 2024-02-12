package org.skyve.admin.web;

import org.skyve.CORE;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.views.PublicFacesView;
import org.skyve.persistence.Persistence;

import jakarta.enterprise.context.RequestScoped;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Named;
import modules.admin.Communication.CommunicationBizlet;
import modules.admin.Subscription.SubscriptionBizlet;

@RequestScoped
@Named("adminUnsubscribe")
public class UnsubscribeView extends PublicFacesView {
	private static final long serialVersionUID = 6713621260342289323L;

	// indicates if the RSVP processing on HTTP GET was successful or not
	private boolean success = false;

	public boolean isSuccess() {
		return success;
	}

	@Override
	public void preRender() {
		super.preRender();
		
		new FacesAction<Void>() {
			@Override
			public Void callback() throws Exception {
				FacesContext fc = FacesContext.getCurrentInstance();
				if (! fc.isPostback()) {
					initialise();
					
					String bizCustomer = getBizCustomerParameter();
					String communicationId = getBizIdParameter();
					String receiverIdentifier = fc.getExternalContext().getRequestParameterMap().get("r");

					Persistence p = CORE.getPersistence();
					boolean communicationExists = CommunicationBizlet.anonymouslyCommunicationExists(p, bizCustomer, communicationId);
					if (communicationExists) {

						boolean subscriptionExists = SubscriptionBizlet.anonymouslySubscriptionExists(p, bizCustomer, communicationId, receiverIdentifier);
						if (!subscriptionExists) {
							SubscriptionBizlet.anonymouslyUnsubscribe(p, bizCustomer, communicationId, receiverIdentifier);
						}

						// return true if the communication checks out - as this
						// is a legitimate request
						success = true;
					}
				}

				return null;
			}
		}.execute();
	}
}
