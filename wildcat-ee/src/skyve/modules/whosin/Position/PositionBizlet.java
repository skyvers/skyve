package modules.whosin.Position;

import modules.whosin.domain.Position;

import org.skyve.metadata.model.document.Bizlet;

public class PositionBizlet extends Bizlet<Position> {

	/**
	 * 
	 */
	private static final long serialVersionUID = 441801393959532437L;

	@Override
	public void preSave(Position bean) throws Exception {

		if (bean.getReportsTo() == null) {
			bean.setBizParentId(null);
		} else {
			bean.setBizParentId(bean.getReportsTo().getBizId());
		}

		super.preSave(bean);
	}

	@Override
	public void postLoad(Position bean) throws Exception {
		if (bean.getParent() != null) {
			bean.setReportsTo(bean.getParent());
		}
		super.postLoad(bean);
	}

}
