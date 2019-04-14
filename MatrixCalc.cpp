#if !defined(MATRIX_H)
#define MATRIX_H
#include <stdio.h>
#include <iostream>
#include <tchar.h>
#include <math.h>
#include <stdlib.h>
using namespace std;
class Matrix{
    private:
        int row;
        int col;
        char name[10];    
    public:
    	Matrix();
        double **Data;
        Matrix(const char *m_name,int rows,int cols) : row(rows),col(cols){
            strcpy(name,m_name);
            Data=new double*[row];
            for(int i=0;i<row;i++)
                Data[i]=new double[col];
            for(int i=0;i<row;i++){
                for(int j=0;j<col;j++){
                    Data[i][j]=0.0;
                }
            }
        }
        Matrix(const Matrix &other){
            strcpy(name,other.name);
            row=other.row;
            col=other.col;
            Data=new double*[row];
            for(int i=0;i<row;i++)
                Data[i]=new double[col];
            for(int i=0;i<row;i++){
                for(int j=0;j<col;j++){
                    Data[i][j]=other.Data[i][j];
                }
            }
        }
        ~Matrix(){
            for(int i = 0; i < row; i++)
            	delete[] Data[i];
            delete[] Data;
            row=col = 0;
        }
        double Determinant()
        {
            double det = 0;
            double **pd = Data;
            switch (row){
            	case 1:{
            		det= pd[0][0];
            		return det;
					break;
				}
                case 2:{
                    det = pd[0][0] * pd[1][1] - pd[0][1] * pd[1][0];
                    return det;
                	break;
				}
                case 3:{
                    double a = pd[0][0];
                    double b = pd[0][1];
                    double c = pd[0][2];
                    double d = pd[1][0];
                    double e = pd[1][1];
                    double f = pd[1][2];
                    double g = pd[2][0];
                    double h = pd[2][1];
                    double i = pd[2][2];
                    det=a*((e*i)-(h*f))-b*((d*i)-(g*f))+c*((d*h)-(g*e));
                    return det;
                	break;
				}
                case 4:{
                    Matrix *temp[4];
                    for(int i=0;i<4;i++)
                        temp[i]=new Matrix("", 3, 3);
                    for (int k=0;k<4;k++){
                        for (int i=1;i<4;i++){
                            int j1=0;
                            for (int j=0;j<4;j++){
                                if (k==j)
                                    continue;
                                temp[k]->Data[i-1][j1++]=this->Data[i][j];
                            }
                        }
                    }
                    double local_a= this->Data[0][0]*temp[0]->Determinant();
                    double local_b= this->Data[0][1] * temp[1]->Determinant();
                    double local_c= this->Data[0][2] * temp[2]->Determinant();
                    double local_d= this->Data[0][3] * temp[3]->Determinant();
                    det=local_a-local_b+local_c-local_d;
                    return det;
                	break;
				}
                case 5:
                {
                    Matrix *temp[5];
                    for (int i = 0; i < 5; i++)
                        temp[i] = new Matrix("", 4, 4);
                    for (int k = 0; k < 5; k++)
                    {
                        for (int i = 1; i < 5; i++)
                        {
                            int j1 = 0;
                            for (int j = 0; j < 5; j++)
                            {
                                if (k == j)
                                    continue;
                                temp[k]->Data[i - 1][j1++]= this->Data[i][j];
                            }
                        }
                    }
                    double local_a= this->Data[0][0] * temp[0]->Determinant();
                    double local_b= this->Data[0][1] * temp[1]->Determinant();
                    double local_c= this->Data[0][2] * temp[2]->Determinant();
                    double local_d= this->Data[0][3] * temp[3]->Determinant();
                    double local_e= this->Data[0][4] * temp[4]->Determinant();
                    double det=local_a+local_b-local_c+local_d-local_e;
                    return det;
                    break;
                }
                case 6:{
                	goto LabelDet;	
					break;
				}
                case 7:{
                	goto LabelDet;
					break;
				}
                case 8:{
                	goto LabelDet;
					break;
				}
                case 9:{
                	goto LabelDet;
					break;
				}
                case 10:{
                	goto LabelDet;
					break;
				}
                LabelDet:	
                default:{
                    int DIM = row;
                    Matrix **temp = new Matrix*[DIM];
                    for(int i=0;i<DIM;i++)
					    temp[i] = new Matrix("", DIM - 1, DIM - 1);
                    for(int k=0;k<DIM;k++){
                        for(int i=1;i<DIM;i++){
                            int j1=0;
                            for (int j=0;j<DIM;j++){
                                if (k==j)
                                    continue;
                                temp[k]->Data[i-1][j1++]=this->Data[i][j];
                            }
                        }
                    }
                    double det=0;
                    for (int k=0;k<DIM;k++){
                        if((k%2)==0)
                            det=det+(this->Data[0][k]*temp[k]->Determinant());
                        else
                            det=det-(this->Data[0][k]*temp[k]->Determinant());
                    }
                    for(int i=0;i<DIM;i++)
                        delete temp[i];
                    delete[] temp;
                    return det;
                	break;
				}
            }
        }
        Matrix& operator =(const Matrix &other){
            if (this->row!=other.row || this->col!=other.col){
                cout<<"[WARNING]:change of rows and coumns-assignment"<<endl;;
            }
            for(int i=0;i<row;i++)
                delete[] Data[i];
            delete[] Data;
            row=col=0;
            strcpy(name, other.name);
            row=other.row;
            col=other.col;
            Data = new double*[row];
            for(int i=0;i<row;i++)
                Data[i] = new double[col];
            for(int i=0;i<row;i++){
                for(int j=0;j<col;j++){
                    Data[i][j] = other.Data[i][j];
                }
            }
            return *this;
        }
        Matrix CoFactor(){
            Matrix cofactor("COF", row, col);
            if(row!=col)
                return cofactor;
            if(row<2)
                return cofactor;
            else if(row==2){
                cofactor.Data[0][0]=Data[1][1];
                cofactor.Data[0][1]=-Data[1][0];
                cofactor.Data[1][0]=-Data[0][1];
                cofactor.Data[1][1]=Data[0][0];
                return cofactor;
            }
            else if (row>=3){
                int DIM = row;
                Matrix ***temp = new Matrix**[DIM];
                for (int i=0;i<DIM;i++)
                    temp[i]=new Matrix*[DIM];
                for (int i=0;i<DIM;i++)
                    for (int j=0;j<DIM;j++)
                        temp[i][j]=new Matrix("",DIM-1,DIM-1);
                for (int k1=0;k1<DIM;k1++){
                    for (int k2=0;k2<DIM;k2++){
                        int i1=0;
                        for (int i=0;i<DIM;i++){
                            int j1=0;
                            for (int j=0;j<DIM;j++){
                                if (k1==i || k2==j)
                                    continue;
                                temp[k1][k2]->Data[i1][j1++]=this->Data[i][j];
                            }
                            if (k1 != i)
                                i1++;
                        }
                    }
                }
                bool flagPositive = true;
                for(int k1=0;k1<DIM;k1++){
                    flagPositive=((k1%2)==0);
                    for(int k2=0;k2<DIM;k2++){
                        if (flagPositive == true){
                            cofactor.Data[k1][k2]=temp[k1][k2]->Determinant();
                            flagPositive = false;
                        }
                        else{
                            cofactor.Data[k1][k2]
                                    = -temp[k1][k2]->Determinant();
                            flagPositive = true;
                        }
                    }
                }
                for(int i=0;i<DIM;i++)
                    for(int j=0;j<DIM;j++)
                        delete temp[i][j];
                for(int i=0;i<DIM;i++)
                    delete[] temp[i];
                delete[] temp;
            }
            return cofactor;
        }
        Matrix Adjoint(){
            Matrix cofactor("COF", row, col);
            Matrix adj("ADJ", row, col);
            if (row!=col)
                return adj;
            cofactor=this->CoFactor();
            // adjoint is transpose of a cofactor of a matrix
            for(int i=0;i<row;i++){
                for(int j=0;j<col;j++){
                    adj.Data[j][i]=cofactor.Data[i][j];
                }
            }
            return adj;
        }
        Matrix Transpose(){
            Matrix trans("TR",col,row);
            for(int i=0;i<row;i++){
                for (int j=0;j<col;j++){
                    trans.Data[j][i]=Data[i][j];
                }
            }
            return trans;
        }
        Matrix Inverse(){
            Matrix cofactor("COF", row, col);
            Matrix inv("INV", row, col);
            if (row != col)
                return inv;
            // to find out Determinant
            double det = Determinant();
            if(det==0){
            	cout<<"Inverse of a matrix cannot be calculated since det=0"<<endl;
            	goto Line272;
			}
            cofactor = this->CoFactor();
            // inverse = transpose of (cofactor) / Determinant
            for (int i = 0; i < row; i++){
                for (int j = 0; j < col; j++){
                    inv.Data[j][i] = cofactor.Data[i][j] / det;
                }
            }
            Line272:
            return inv;
        }
        Matrix operator +(const Matrix &other){
            if (this->row != other.row || this->col != other.col)
            {
                cout<<"Addition is not possible"<<endl;
                terminate();
				return *this;
            }
            Matrix result("", row, col);
            for(int i=0;i<row;i++){
                for(int j=0;j<col;j++){
                    result.Data[i][j]=this->Data[i][j]+ other.Data[i][j];
                }
            }
            return result;
        }
        Matrix operator -(const Matrix &other){
            if (this->row != other.row || this->col != other.col)
            {
                cout<<"Subtraction is not possible"<<endl;
               terminate();
				return *this;
            }
            Matrix result("", row, col);
            for (int i=0;i<row;i++){
                for (int j=0;j<col;j++){
                    result.Data[i][j]=this->Data[i][j]-other.Data[i][j];
                }
            }
            return result;
        }
        Matrix operator *(const Matrix &other){
            if (this->col != other.row)
            {
                cout<<"Multiplication is not possible"<<endl;
               	terminate();
				return *this;
            }
            Matrix result("", this->row, other.col);
            for(int i=0;i<this->row;i++){
                for(int j=0;j<other.col;j++){
                    for(int k=0;k<this->col;k++){
                        result.Data[i][j] += this->Data[i][k]* other.Data[k][j];
                    }
                }
            }
            return result;
        }
        
        friend istream& operator >>(istream &is, Matrix &m);
        friend ostream& operator <<(ostream &os, const Matrix &m);
        
};
istream& operator >>(istream &is, Matrix &m){
    cout << "Enter elements"<<endl;
    for(int i=0;i<m.row;i++){
        for(int j=0;j<m.col;j++){
            is>>m.Data[i][j];
        }
    }
    return is;
}
ostream& operator <<(ostream &os, const Matrix &m){
    for(int i=0;i<m.row;i++){
        os<<" | ";
        for(int j=0;j<m.col;j++){
            char buf[32];
            double data = m.Data[i][j];
            if (m.Data[i][j] > -0.00001 && m.Data[i][j] < 0.00001)
                data = 0;//negligible value consider it to zero
            sprintf(buf, "%1.2lf ", data);//%1.21f is used for spacing
            os << buf;
        }
        os <<"| "<<endl;
    }
    os << endl;
    return os;
}
#endif
int main(){
	cout<<"			_________________________"<<endl;
	cout<<"			----MATRIX CALCULATOR----"<<endl;
	cout<<"			_________________________"<<endl;
	
	int r1,c1,r2,c2,r3,c3,r4,c4;
	int choice;
	
	do{
		cout<<"		_________________________"<<endl;
		cout<<"\t\tEnter ::"<<endl;
		cout<<"\t\t| 1.ADD Matrices "<<endl;
		cout<<"\t\t| 2.SUBTRACT Matrices "<<endl;
		cout<<"\t\t| 3.MULTIPLY Matrices "<<endl;
		cout<<"\t\t| 4.TRANSPOSE a Matrix "<<endl;
		cout<<"\t\t| 5.DETERMINANT of a Matrix "<<endl;
		cout<<"\t\t| 6.ADJOINT Matrix "<<endl;
		cout<<"\t\t| 7.COFACTOR matrix "<<endl;
		cout<<"\t\t| 8.INVERSE Matrix "<<endl;
		cout<<"\t\t| 9.'n'th POWER of a Matrix "<<endl;
		cout<<"\t\t| 10.TERMINATE (or) EXIT"<<endl;
		cout<<"\t\tYour choice :: ";
		cin>>choice;
		switch(choice){
			case 1:{
				int n;
				cout<<"Enter the number of matrices to be added"<<endl;
				cin>>n;
				if(n==2){
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r1>>c1;
					Matrix a("A",r1,c1);
					cin>>a;
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r2>>c2;
					Matrix b("B",r2,c2);
					cin>>b;
					
					cout<<"Matrix A"<<endl;
					cout<<a;
					cout<<"Matrix B"<<endl;
					cout<<b;
					if(r1==r2 && c1==c2){
						Matrix c=a+b;
						cout<<"[RESULT]:ADDITION of two matrices"<<endl;
						cout<<c;
					}
					else
						goto begin;
				}
				else if(n==3){
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r1>>c1;
					Matrix a("A",r1,c1);
					cin>>a;
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r2>>c2;
					Matrix b("B",r2,c2);
					cin>>b;
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r3>>c3;
					Matrix c("C",r3,c3);
					cin>>c;
					cout<<"Matrix A"<<endl;
					cout<<a;
					cout<<"Matrix B"<<endl;
					cout<<b;
					cout<<"Matrix C"<<endl;
					cout<<c;
					
					if(r1==r2 && r2==r3 && c1==c2 &&c2==c3){
						Matrix d=a+b;
					Matrix e=c+d;
						cout<<"[RESULT]:ADDITION of three matrices"<<endl;
						cout<<e;
					}
					else
						goto begin;
				}
				else if(n==4){
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r1>>c1;
					Matrix a("A",r1,c1);
					cin>>a;
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r2>>c2;
					Matrix b("B",r2,c2);
					cin>>b;
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r3>>c3;
					Matrix c("C",r3,c3);
					cin>>c;
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r4>>c4;
					Matrix d("D",r4,c4);
					cin>>d;
					cout<<"Matrix A"<<endl;
					cout<<a;
					cout<<"Matrix B"<<endl;
					cout<<b;
					cout<<"Matrix C"<<endl;
					cout<<c;
					cout<<"Matrix D"<<endl;
					cout<<d;
					
					if(r1==r2 && r2==r3 && r3==r4 && c1==c2 && c2==c3 && c3==c4){
						Matrix e=a+b;
					Matrix f=c+d;
					Matrix g=e+f;
						cout<<"[RESULT]:ADDITION of four Matrices"<<endl;
						cout<<g;
					}
					else
						goto begin;
				}
				break;
			}
			case 2:{
				int n;
				cout<<"Enter the number of matrices to be subtracted"<<endl;
				cin>>n;
				if(n==2){
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r1>>c1;
					Matrix a("A",r1,c1);
					cin>>a;
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r2>>c2;
					Matrix b("B",r1,c1);
					cin>>b;
					
					cout<<"Matrix A"<<endl;
					cout<<a;
					cout<<"Matrix B"<<endl;
					cout<<b;
					if(r1==r2&&c1==c2){
						Matrix c=a-b;
						cout<<"[RESULT]:SUBTRACTION of two matrices"<<endl;
						cout<<c;
					}
					else
						goto begin;
				}
				else if(n==3){
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r1>>c1;
					Matrix a("A",r1,c1);
					cin>>a;
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r2>>c2;
					Matrix b("B",r2,c2);
					cin>>b;
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r3>>c3;
					Matrix c("C",r3,c3);
					cin>>c;
					cout<<"Matrix A"<<endl;
					cout<<a;
					cout<<"Matrix B"<<endl;
					cout<<b;
					cout<<"Matrix C"<<endl;
					cout<<c;
					
					if(r1==r2 && r2==r3 && c1==c2 &&c2==c3){
						Matrix d=a-b;
						Matrix e=d-c;
						cout<<"[RESULT]:SUBTRACTION of three matrices"<<endl;
						cout<<e;
					}
					else
						goto begin;
				}
				else if(n==4){
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r1>>c1;
					Matrix a("A",r1,c1);
					cin>>a;
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r2>>c2;
					Matrix b("B",r1,c1);
					cin>>b;
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r3>>c3;
					Matrix c("C",r1,c1);
					cin>>c;
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r4>>c4;
					Matrix d("D",r1,c1);
					cin>>d;
					cout<<"Matrix A"<<endl;
					cout<<a;
					cout<<"Matrix B"<<endl;
					cout<<b;
					cout<<"Matrix C"<<endl;
					cout<<c;
					cout<<"Matrix D"<<endl;
					cout<<d;
					
					if(r1==r2 && r2==r3 && r3==r4 && c1==c2 && c2==c3 && c3==c4){
						Matrix e=a-b;
						Matrix f=c-d;
						Matrix g=e-f;
						cout<<"[RESULT]:SUBTRACTION of four Matrices"<<endl;
						cout<<g;
					}
					else
						goto begin;
				}
				break;
			}
			case 3:{
			int n;
				cout<<"Enter the number of matrices to be multiplied"<<endl;
				cin>>n;
				if(n==2){
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r1>>c1;
					Matrix a("A",r1,c1);
					cin>>a;
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r2>>c2;
					Matrix b("B",r2,c2);
					cin>>b;
					
					cout<<"Matrix A"<<endl;
					cout<<a;
					cout<<"Matrix B"<<endl;
					cout<<b;
					if(r2==c1){
						Matrix c=a*b;
						cout<<"[RESULT]:MULTIPLICATION of two matrices"<<endl;
						cout<<c;
					}
					else
						goto begin;
				}
				else if(n==3){
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r1>>c1;
					Matrix a("A",r1,c1);
					cin>>a;
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r2>>c2;
					Matrix b("B",r2,c2);
					cin>>b;
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r3>>c3;
					Matrix c("C",r3,c3);
					cin>>c;
					cout<<"Matrix A"<<endl;
					cout<<a;
					cout<<"Matrix B"<<endl;
					cout<<b;
					cout<<"Matrix C"<<endl;
					cout<<c;
					if(c1==r2 && c2==r3){
						Matrix d=a*b;
						Matrix e=d*c;
						cout<<"[RESULT]:MULTIPLICATION of three matrices"<<endl;
						cout<<e;
					}
					else
						goto begin;
				}
				else if(n==4){
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r1>>c1;
					Matrix a("A",r1,c1);
					cin>>a;
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r2>>c2;
					Matrix b("B",r2,c2);
					cin>>b;
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r3>>c3;
					Matrix c("C",r3,c3);
					cin>>c;
					cout<<"Enter the size of the matrix"<<endl;
					cin>>r4>>c4;
					Matrix d("D",r4,c4);
					cin>>d;
					cout<<"Matrix A"<<endl;
					cout<<a;
					cout<<"Matrix B"<<endl;
					cout<<b;
					cout<<"Matrix C"<<endl;
					cout<<c;
					cout<<"Matrix D"<<endl;
					cout<<d;
					if(c1==r2 && c2==r3 && c3==r4){
						Matrix e=a*b;
						Matrix f=c*d;
						Matrix g=e*f;
						cout<<"[RESULT]:MULTIPLICATION of four matrices"<<endl;
						cout<<g;
					}
					else
						goto begin;
				}
				break;
			}
			case 4:{
				cout<<"Enter the size of the matrix"<<endl;
				cin>>r1>>c1;
				Matrix a("A",r1,c1);
				cin>>a;
				Matrix t=a.Transpose();
				cout<<"Matrix A"<<endl;
				cout<<a;
				cout<<"[RESULT]:TRANSPOSE of Matrix A"<<endl;
				cout<<t;
				break;
			}
			case 5:{
				cout<<"Enter the size of the matrix"<<endl;
				cin>>r1>>c1;
				Matrix a("A",r1,c1);
				cin>>a;
				int det=a.Determinant();
				cout<<"[RESULT]:Determinant = "<<det<<endl;
				break;
			}
			case 6:{
				cout<<"Enter the size of the matrix"<<endl;
				cin>>r1>>c1;
				Matrix a("A",r1,c1);
				cin>>a;
				Matrix t=a.Adjoint();
				cout<<"Matrix A"<<endl;
				cout<<a;
				cout<<"[RESULT]:ADJOINT of Matrix A"<<endl;
				cout<<t;
				break;
			}
			case 7:{
				cout<<"Enter the size of the matrix"<<endl;
				cin>>r1>>c1;
				Matrix a("A",r1,c1);
				cin>>a;
				cout<<"Matrix A"<<endl;
				cout<<a;
				Matrix t=a.CoFactor();
				cout<<"[RESULT]:CO-FACTOR of Matrix A"<<endl;
				cout<<t;
				break;
			}
			case 8:{
				cout<<"Enter the size of the matrix"<<endl;
				cin>>r1>>c1;
				Matrix a("A",r1,c1);
				cin>>a;
				Matrix inv=a.Inverse();
				int d=a.Determinant();
				if(d==0)
					goto begin;
				cout<<"Matrix A"<<endl;
				cout<<a;
				cout<<"[RESULT]:INVERSE of Matrix A"<<endl;
				cout<<inv;
				Matrix unitMatrix=(a*inv);
				cout<<"[WARNING]:Verification"<<endl;
				cout<<"Verification A * A-Inverse must be an identity matrix"<<endl;
				cout<<unitMatrix;
				break;
			}
			case 9:{
				cout<<"Enter the size of a matrix"<<endl;
				cin>>r1>>c1;
				int n;
				if(r1==c1){
					Matrix a("A",r1,c1);
					cin>>a;
					cout<<"nth power of (n=?) :: ";
					cin>>n;
					Matrix r=a;
					for(int i=1;i<n;i++)
						r=r*a;
					cout<<"Matrix A"<<endl;
					cout<<a;
					cout<<"[RESULT]:'n'th POWER of a Matrix"<<endl;
					cout<<r;
				}
				break;
			}
			case 10:{
				cout<<"[WARNING]:Program terminated"<<endl;
				exit(0);
				break;
			}
			default:{
				cout<<"___________________________________"<<endl;
				cout<<"[WARNING]:Incorrect option selected"<<endl;
				break;
			}
		}
		cout<<"[INFO]:Operation is succesfully performed"<<endl;
	}while(1);
	begin:
		cout<<"[WARNING]:Operation cannot be performed"<<endl;
}
