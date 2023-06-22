
# FS-final project
금융통계 기말 프로젝트

작성자: 이수민

## 목차
1. 프로젝트 소개
2. 용어 소개
3. 데이터 분석

## 프로젝트 소개
주제 선정 동기:

네이버 증권 페이지를 공부하며 기업가치를 나타내는 다양한 값들을 알게 되었고, 그 값을 통해 기업의 가치 판단이 이루어진다는 사실을 알게 되었다. 그러던 중 PER 값만을 보고 PER의 상대적 가치를 판단할 수 없다 생각이 되어 해당 업종 평균 PER을 기준으로 비교하여 업종 평균 PER 대비 해당 값이 큰지 작은지 비교하여 특정 기업의 가치가 고평가되어 있는지 저평가 되어있는지 분석하였다.


## 용어 소개

* PER
```
주가수익비율로 기업 이익에 비해 주가가 어느 정도 수준인지 나타낸 비율

주가 / 주당 순이익(EPS) 로 계산하며 1주당 수익의 몇 배가 되는지 나타냄

* EPS = 당기 순이익 / 발행된 주식 수
```

* 시가총액
```
발행주식수와 주가를 곱하여 계산

상장주식을 시가로 평가한 것으로 회사의 규모를 평가할 때 사용
```

## 데이터 분석

[네이버 증권 업종별 페이지](https://finance.naver.com/sise/sise_group.naver?type=upjong)에서 비교하고자 하는 업종을 선택한다. 
**'게임엔터테인먼트'** 업종을 이용하여 분석하였다.

### 전체 종목의 PER 시각화
![종목별 PER](https://github.com/soomeng/FS-final-project/assets/127038026/942016a3-d621-4fc7-a677-15c256b187ca)

PER이 음수인 값을 가지는 것은 **주당순이익이 마이너스인 경우**이다. 
우리나라 주식들은 대부분 **경기변동형 주식**으로 구성되어 있어 급격한 경기변동이 오는 경우 PER가 마이너스가 나오는 경우가 종종 있다. 

이런 투자에서 오는 PER 마이너스가 있기 때문에 PER이 마이너스라고 해서 해당 종목이 좋다, 나쁘다 해석할 수 없다.  주당순이익이 일시적인 마이너스 값인 경우 어느 정도 하락 후 기대감으로 인해 주가가 강하게 상승하는 경우도 존재한다.

---
### 특정 종목의 가치 평가

* 가정: 주가가 큰 기업이 고평가 되어있을 것으로 가정하였다.   이때 시가총액은 주가와 발행주식수를 곱하여 계산하기 때문에 시가총액이 큰 종목을 선택하여 업종 평균 PER을 비교할 것이다. 

업종 전체 평균 PER과 시가총액이 가장 큰 종목의 PER을 비교 필요한 데이터 항목인 시가총액과 PER를 불러오기 위하여 docker를 사용하였다.

웹스크래핑을 통하여 종목명, 시가총액, PER 데이터를 얻어올 수 있다. 


아래에서 PER 비율이라는 용어의 사용을 위해 해당 용어를 정의하자.
```
 PER 비율 = 종목의 PER / 해당 업종 평균 PER
```
---
### 시가총액이 가장 큰 '크래프톤'을 전체 업종 평균과 비교

* 결과

|Company | PER  |AVG_PER|PER_Ratio|
|--------|------|-------|---------|
|크래프톤 |18.77 |24.7   | 0.759919|

게임엔터테인먼트 전체 업종의 평균 PER은 24.7이며, 크래프톤 종목의 PER은 18.77로 나타난다.

이를 통해 얻은 PER 비율이 **0.759919**로 1보다 작은 값으로, '크래프톤' 종목의 주가는 업종 평균에 비해 비교적 **저평가**되어 있는 것으로 볼 수 있다.


* 의문점?
주가가 높을수록 해당 종목이 고평가 되어있을 것이라 가정하였는데 전혀 반대의 결과를 얻었다.
--> 그렇다면 주가나 시가총액이 PER과 관계가 없는 값인가?

---
### PER과 시가총액의 상관관계 분석

![시가총액과 PER의 상관관계](https://github.com/soomeng/FS-final-project/assets/127038026/31674058-fd38-43e7-8674-fb3102eac818)

산점도를 통해 시가총액이 증가함에 따라 PER가 감소하는 경향을 볼 수 있다.

또한, 상관계수가 **-0.1070046**로 매우 약한 음의 상관관계를 가진다는 것을 알 수 있다.(상관계수의 값이 0에 가까울수록 두 변수 간의 선형관계가가 약함)



>분석 결과에 대한 고찰
위에 설명한 PER와 시가총액의 정의를 이용해 설명 가능하다.
>1.  EPS(주당순이익)를 이용해 PER이 산정되는데, EPS라는 개념은 해당 기업의 재무제표상 당기순이익을 기반으로 산정된 수치
재무제표상 자본 및 부채는 공정가치와 장부가치를 기반으로 작성
>2. 시가총액은 말그대로 정통한 시장에서의 해당 기업의 객관적인 시장가치성격을 가짐
공정가치: 매도, 매수자 간에 합의된 가치
장부가치: 자산이나 부채의 회계 장부 가치
시장가치: 시장에서 수요와 공급에 의해 결정되는 가치

>결론: 재무제표상 자본총계와 시가총액이 필연적으로 다를 수 밖에 없기에 상관계수가 낮게 나타날 수 밖에 없다.

---


### 게임엔터테인먼트 업종의 전체 주식 가치평가 시각화

![종목 가치평가](https://github.com/soomeng/FS-final-project/assets/127038026/556220fc-ad30-4c32-89ce-11c26781ce67)

게임엔터테인먼트 업종의 모든 종목을 업종 평균대비 1보다 큰 값인지 작은 값인지 시각화를 통해 분석하였다.

회색 수평선이 1 기준을 나타낸다. 1보다 큰 종목을 파란색(고평가)로, 작은 종목을 빨간색(저평가)로 나타내주었다.

이때, PER이 **음수**인 값은 현재 고평가, 저평가를 판단할 수 없기 때문에 검정색으로 표시하여 분석에서 제외하였다.

만약 투자를 결정한다면 저평가 되어있는 종목을 택하여 ROE, PBR 등 기업 가치를 판단할 수 있는 값을 함께 분석하여 투자를 결정해야한다. 단순히 PER만을 보고 투자 결정하기에는 무리가 있다.


---

### 고평가된 주가의 실제 변화 관찰
![액토즈소프트의 주가 변화](https://github.com/soomeng/FS-final-project/assets/127038026/4db253f1-81d2-4dba-9709-73a923216f94)

실제로 액토즈소프트의 주가는 계속해서 **감소**하고 있으며, 이는 액토즈소프트의 주가가 실제보다 **고평가**되어있기 때문에 계속해서 감소하고 있는 것으로 볼 수 있다.