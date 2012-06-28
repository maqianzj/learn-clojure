package protocol;

/**
 * Э��ͷ������
 * @author maqian
 * @version 1.0
 */
public interface IHeadAccessor {

	/**
	 * Э��ͷ(��ݳ���32λ)
	 * @return
	 */
	public abstract int getHead();
	public abstract void setHead(int head);
	
	/**
	 * Э��汾��
	 * @return
	 */
	public abstract int getVersion();
	public abstract void setVersion(int version);
	
	/**
	 * ��ԭ��ݻ��߼���ǰ����ݽ�����䣨AES������16�ֽڶ��룩��
	 * 0:�����
	 * 1:16�ֽ����
	 * @return
	 */
	public abstract int getPadding();
	public abstract void setPadding(int padding);
	
	/**
	 * ��������ݱ�����ص�����Ƿ񾭹�ֿ鷢�͡�
	 * 0:����ݱ���δ���ֿ�
	 * 1:�ֿ飬��һ����ݱ�
	 * 2:�ֿ飬������ݱ�
	 * 3:δ����
	 * @return
	 */
	public abstract int getSplit();
	public abstract void setSplit(int split);
	
	/**
	 * ��ʶ����ݱ��Ƿ񾭹���ܡ����ܷ�����ϵͳ����ָ������ݱ���ʹ�õļ��ܷ�����Э�鱾��ȷ����
	 * 0:�Ǽ���
	 * 1:����
	 * @return
	 */
	public abstract int getCrypt();
	public abstract void setCrypt(int crypt);
	
	/**
	 * ��ʾ����ݱ��Ƿ񾭹�ѹ��
	 * 0:��ʾδѹ��
	 * 1:��ʾѹ��
	 * @return
	 */
	public abstract int getCompress();
	public abstract void setCompress(int compress);
	
	/**
	 * 1��ʾ���յ������ʱӦ�÷���Ӧ��(UDP��Ч)
	 * @return
	 */
	public abstract int getAck();
	public abstract void setAck(int ack);
	
	/**
	 * 1��ʾЭ��Ҫ�󷵻ز����ΪӦ���
	 * @return
	 */
	public abstract int getAckparam();
	public abstract void setAckparam(int ackParam);
	
	/**
	 * ������Ự���ʱ��ʾ�Ự����
	 * �������Ϊý�����ʱ��ʾ���֡����
	 * @return
	 */
	public abstract int getType();
	public abstract void setType(int type);
	
	/**
	 * ����Э���ʶ
	 * @return
	 */
	public abstract int getTag();
	public abstract void setTag(int tag);
	
	/**
	 * ����Э������
	 * @return
	 */
	public abstract int getCommand();
	public abstract void setCommand(int command);
	
	
	/**
	 * �û���ݣ�8λ��
	 * @return
	 */
	public abstract int getUserdata();
	public abstract void setUserdata(int userData);

	

	

	

	

	

	

	

	
	
	

	

}