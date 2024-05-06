#include <iostream>
#include <Eigen/Dense>

using Eigen::MatrixXd;
using Eigen::VectorXd;

int main()
{

    std::cout << "Simple Matrix" << std::endl;
    MatrixXd m(2,2);
    m(0,0) = 3;
    m(1,0) = 2.5;
    m(0,1) = -1;
    m(1,1) = m(1,0) + m(0,1);
    std::cout << m << std::endl;

    std::cout << "Matrix Math" << std::endl;
    MatrixXd m1 = MatrixXd::Random(3,3);
    m1 = (m1 + MatrixXd::Constant(3,3,1.2)) * 50;
    std::cout << "m1 =" << std::endl << m1 << std::endl;
    VectorXd v(3);
    v << 1, 2, 3;
    std::cout << "m1 * v =" << std::endl << m1 * v << std::endl;
}