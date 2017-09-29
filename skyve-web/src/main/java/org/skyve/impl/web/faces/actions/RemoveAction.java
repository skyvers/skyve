package org.skyve.impl.web.faces.actions;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.util.Util;

/**
 * Remove an element from an array.
 * The onRemovedHandlers event actions are processed here also.
 */
public class RemoveAction extends FacesAction<Void> {
	private FacesView<? extends Bean> facesView;
	private String collectionName;
	private String elementBizId;
	private List<String> removedHandlerActionNames; // "true/false" means rerender with/without client validation
	
	public RemoveAction(FacesView<? extends Bean> facesView,
							String collectionName,
							String elementBizId,
							List<String> removedHandlerActionNames) {
		this.facesView = facesView;
		this.collectionName = collectionName;
		this.elementBizId = elementBizId;
		this.removedHandlerActionNames = removedHandlerActionNames;
	}

	@Override
	public Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("RemoveAction - collectionName=" + collectionName + " : elementBizId=" + elementBizId);

		Bean bean = facesView.getBean();
		String viewBinding = facesView.getViewBinding();
		if ((collectionName != null) && (elementBizId != null)) { // inline remove
			Bean beanToRemove = ActionUtil.getTargetBeanForViewAndCollectionBinding(facesView, collectionName, elementBizId);

			StringBuilder collectionBinding = new StringBuilder(32);
			if (viewBinding != null) {
				collectionBinding.append(viewBinding).append('.');
			}
			collectionBinding.append(collectionName);
			@SuppressWarnings("unchecked")
			List<Bean> list = (List<Bean>) BindUtil.get(bean, collectionBinding.toString());

			list.remove(beanToRemove);
			if (beanToRemove instanceof ChildBean<?>) {
				((ChildBean<?>) beanToRemove).setParent(null);
			}
			
			// fire onRemovedHandlers after removal
			if (removedHandlerActionNames != null) {
				for (String removedHandlerActionName : removedHandlerActionNames) {
					if (Boolean.TRUE.toString().equals(removedHandlerActionName)) {
						if (UtilImpl.FACES_TRACE) Util.LOGGER.info("RemoveAction - execute removed handler rerender with client validation");
						new RerenderAction<>(facesView, collectionName, true).execute();
					}
					else if (Boolean.FALSE.toString().equals(removedHandlerActionName)) {
						if (UtilImpl.FACES_TRACE) Util.LOGGER.info("RemoveAction - execute removed handler rerender with no client validation");
						new RerenderAction<>(facesView, collectionName, false).execute();
					}
					else {
						if (UtilImpl.FACES_TRACE) Util.LOGGER.info(String.format("RemoveAction - execute removed handler [%s] action", removedHandlerActionName));
						new ExecuteActionAction<>(facesView, removedHandlerActionName, null, null).execute();
					}
				}
			}
		}
		else { // Remove on zoomed view
			int lastCollectionindex = viewBinding.lastIndexOf("ElementById(");
			@SuppressWarnings("unchecked")
			List<Bean> list = (List<Bean>) BindUtil.get(bean, viewBinding.substring(0, lastCollectionindex));
			Bean beanToRemove = (Bean) BindUtil.get(bean, viewBinding);
			list.remove(beanToRemove);
			if (beanToRemove instanceof ChildBean<?>) {
				((ChildBean<?>) beanToRemove).setParent(null);
			}
			
			ZoomOutAction.zoomOut(facesView);
		}

	    return null;
	}
}
